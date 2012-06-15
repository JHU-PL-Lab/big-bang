{-# LANGUAGE Rank2Types
           , GADTs
           , TupleSections #-}
module Language.TinyBang.Types.TypeInference
( inferType
, runTIM
, inferTypeTop
, Gamma
, TypeInferenceError (..)
) where

import Control.Monad.Reader (ask, asks, local)
import Control.Monad.Writer (censor, listen, tell)
import Control.Monad.State (get, put)
import Control.Monad.RWS (RWS, evalRWS)
import Control.Monad.Error (Error, ErrorT, strMsg, throwError, runErrorT)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid (Monoid, mempty)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (foldM)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow ((***))

import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.Types ( (<:)
                                     , (.:)
                                     , Constraint(..)
                                     , Constraints
                                     , TauDown(..)
                                     , CellAlpha
                                     , InterAlpha
                                     , AnyAlpha
                                     , SomeAlpha
                                     , AlphaType
                                     , Alpha(..)
                                     , CellGet(..)
                                     , CellSet(..)
                                     , Cell(..)
                                     , histFIXME
                                     )
import Language.TinyBang.Types.UtilTypes
import Utils.Language.Ast
import Utils.Render.Display

type Gamma = Map Ident CellAlpha
type NextFreshVar = T.AlphaId

-- |An error type for the type inference routine.
data TypeInferenceError
      -- |Indicates that an expression contained an unbound variable.
      = NotClosed Ident
      -- |Indicates that a variable is bound twice in a pattern
      | DoubleBound Ident A.Pattern
      -- |Indicates that a label name is used twice in a multipattern
      | DoubleLabel LabelName A.Pattern
    deriving (Show)
instance Error TypeInferenceError where
    strMsg = error
instance Display TypeInferenceError where
    makeDoc err = case err of
        NotClosed i ->
          text "TypeInference: not closed:" <+>
          (text $ unIdent i)
        DoubleBound i chi ->
          text "TypeInference: variable" <+> makeDoc i
           <+> text "bound twice in the pattern" <+> makeDoc chi
        DoubleLabel n chi ->
          text "TypeInference: label name" <+> makeDoc n
           <+> text "bound twice in the same multipattern in" <+> makeDoc chi

-- |A type alias for the type information monad.
type TIM a = ErrorT TypeInferenceError
                    (RWS Gamma Constraints NextFreshVar)
                    a

runTIM :: TIM a -> Gamma -> NextFreshVar
       -> (Either TypeInferenceError a, T.Constraints)
runTIM t r s = evalRWS (runErrorT t) r s

inferTypeTop :: A.Expr
             -> ( Either TypeInferenceError InterAlpha
                , Constraints
                )
inferTypeTop expr =
  runTIM (inferType expr) Map.empty 0

-- |Performs type inference for a given Big Bang expression.
inferType :: A.Expr -> TIM InterAlpha
inferType ast@(Ast1p1 expr) = do
  gamma <- ask
  let inferred = T.Inferred ast gamma
      tellInferred c = do
        tell1 $ c .: inferred
  case expr of
    A.Var x -> do
      a2 <- maybe (throwError $ NotClosed x) return =<< (asks $ Map.lookup x)
      a1 <- freshVar
      tellInferred $ a2 <: CellGet a1
      return a1
    A.Label n m e -> do
      a1 <- freshVar
      a2 <- inferType e
      a3 <- freshVar
      tellInferred $ Cell a2 <: a3
      tellInferred $ TdLabel n a3 <: a1
      handleMod tellInferred m a2
      return a1
    A.Onion e1 e2 -> do
      a0 <- freshVar
      a1 <- inferType e1
      a2 <- inferType e2
      tellInferred $ TdOnion a1 a2 <: a0
      return a0
    A.Scape pat e -> do
      _ <- tPatCheck pat
      a1 <- freshVar
      (gamma', c', tpat) <- tPatType pat
      -- Since Map.union prefers the leftmost occurrence, the bindings in gamma'
      -- override the ones in gamma
      (a2, c) <- capture (Map.union gamma') e
      let vars = Set.difference
           ( Set.union (extractConstraintTypeVars c)
           $ Set.fromList [alphaWeaken a1, alphaWeaken a2])
           ( Set.fromList $ map alphaWeaken $ Map.elems gamma)
      let scapeType = T.TdScape
            (T.ScapeData (T.ForallVars vars) tpat a2 $ Set.union c' c)
      tellInferred $ scapeType <: a1
      return a1
    A.Appl e1 e2 -> do
      a1' <- freshVar
      a2' <- freshVar
      a1 <- inferType e1
      a2 <- inferType e2
      tellInferred $ a1 <: T.UpFun a1' a2'
      tellInferred $ Cell a2 <: a1'
      tellInferred $ Final a2
      return a2'
    A.PrimInt _ -> do
      a <- freshVar
      tellInferred $ T.PrimInt <: a
      return a
    A.PrimChar _ -> do
      a <- freshVar
      tellInferred $ T.PrimChar <: a
      return a
    A.PrimUnit -> do
      a <- freshVar
      tellInferred $ T.PrimUnit <: a
      return a
    A.OnionSub e s -> do
      a1 <- freshVar
      a2 <- inferType e
      tellInferred $ TdOnionSub a2 s <: a1
      return a1
    A.OnionProj e s -> do
      a1 <- freshVar
      a2 <- inferType e
      tellInferred $ TdOnionProj a2 s <: a1
      return a1
    A.EmptyOnion -> do
      a <- freshVar
      tellInferred $ TdEmptyOnion <: a
      return a
    A.LazyOp op e1 e2 -> do
      a0 <- freshVar
      a1 <- inferType e1
      a2 <- inferType e2
      tellInferred $ T.LazyOp op a1 a2 <: a0
      return a0
    A.EagerOp _ e1 e2 -> do
      -- Currently, all eager operations ignore the result types and just
      -- return a boolean.
      a0 <- freshVar
      _ <- inferType e1
      _ <- inferType e2
      au <- freshVar
      acu <- freshVar
      mapM_ tellInferred $
        [ T.PrimUnit <: au
        , Cell au <: acu
        , T.TdLabel (labelName "True") acu <: a0
        , T.TdLabel (labelName "False") acu <: a0
        ]
      return a0
    A.Def m x e1 e2 -> do
      --TODO: Something about shadowing
      a1 <- inferType e1
      a3 <- freshVar
      a2 <- local (Map.insert x a3) $ inferType e2
      tellInferred $ Cell a1 <: a3
      handleMod tellInferred m a1
      return a2
    A.Assign i e1 e2 -> do
      a3 <- maybe (throwError $ NotClosed i) return =<< (asks $ Map.lookup i)
      a1 <- inferType e1
      a2 <- inferType e2
      tellInferred $ a3 <: CellSet a1
      return a2
    where tell1 :: T.Constraint -> TIM ()
          tell1 = tell . Set.singleton
          -- |Infers the type of the subexpression in an environment modified by
          --  @f@, and prevents constraints from that inference from bubbling
          --  up.
          capture :: (Gamma -> Gamma) -> A.Expr -> TIM (InterAlpha, Constraints)
          capture f e = censor (const mempty) $ listen $ local f $ inferType e
          handleMod f m a = case m of
            Just A.Final -> f $ Final a
            Just A.Immutable -> f $ Immutable a
            Nothing -> return ()

-- |Extracts all type variables from the provided constraints.
extractConstraintTypeVars :: T.Constraints -> Set AnyAlpha
extractConstraintTypeVars c =
    Foldable.foldl foldConstraints Set.empty c
    where foldConstraints set el =
            case el of
                LowerSubtype t a _ -> insertWeak (foldTau set t) a
                UpperSubtype a1 a2 a3 _ -> insert3Weak set a1 a2 a3
                AlphaSubtype a1 a2 _ -> insert2Weak set a1 a2
                CellSubtype a1 a2 _ -> insert2Weak set a1 a2
                CellGetSubtype a1 a2 _ -> insert2Weak set a1 a2
                CellSetSubtype a1 a2 _ -> insert2Weak set a1 a2
                CellAlphaSubtype a1 a2 _ -> insert2Weak set a1 a2
                LazyOpSubtype _ a1 a2 a3 _ -> insert3Weak set a1 a2 a3
                Comparable a1 a2 _ -> insert2Weak set a1 a2
                Final a _ -> insertWeak set a
                Immutable a _ -> insertWeak set a
                T.Bottom _ -> set
          foldTau set tau =
            case tau of
              TdPrim _ -> set
              TdLabel _ a -> insertWeak set a
              TdOnion a1 a2 -> insert2Weak set a1 a2
              TdScape (T.ScapeData (T.ForallVars as) tpat ia cs) ->
                Set.union set $
                Set.difference
                  (insertWeak
                   (Set.union (patternAlphas tpat)
                              (extractConstraintTypeVars cs))
                   ia)
                  as
              TdOnionSub a _ -> insertWeak set a
              TdOnionProj a _ -> insertWeak set a
              TdEmptyOnion -> set
          patternAlphas (T.Pattern a pp) =
            insertWeak (primaryPatternAlphas pp) a
          primaryPatternAlphas p =
            case p of
              T.PatPrim _ -> Set.empty
              T.PatLabel _ a pp -> insertWeak (primaryPatternAlphas pp) a
              T.PatOnion pps -> Set.unions $ map primaryPatternAlphas pps
              T.PatFun -> Set.empty
          insertWeak :: (Alpha a) => Set AnyAlpha -> a -> Set AnyAlpha
          insertWeak set a = Set.insert (alphaWeaken a) set
          insert2Weak :: (Alpha a, Alpha b) => Set AnyAlpha -> a -> b
                      -> Set AnyAlpha
          insert2Weak set a1 a2 =
            insertWeak (insertWeak set a1) a2
          insert3Weak :: (Alpha a, Alpha b, Alpha c) => Set AnyAlpha
                      -> a -> b -> c -> Set AnyAlpha
          insert3Weak set a1 a2 a3 =
            insertWeak (insert2Weak set a1 a2) a3

-- |Creates a fresh type variable for the type inference engine.
freshVar :: (AlphaType a) => TIM (SomeAlpha a)
freshVar = do
    idx <- get
    put $ idx + 1
    return $ T.makeNewAlpha idx

-- |Checks a branch pattern to ensure that it is well-formed.

-- TODO: this doesn't need the full power of TIM since it never reads the
-- environment and more importantly never writes to the constraint set.
tPatCheck :: A.Pattern -> TIM (Set Ident, Set LabelName)
tPatCheck p@(A.Pattern i pp) = do
  (v, l) <- tPatCheckPrimary pp
  (,) <$> disjointVarsetUnion (Set.singleton i) v <*> pure l
  where disjointVarsetUnion :: Set Ident -> Set Ident -> TIM (Set Ident)
        disjointVarsetUnion vs1 vs2 = do -- TIM
          let vsi = Set.delete (ident "_") $ Set.intersection vs1 vs2
          if Set.null vsi
            then return $ Set.union vs1 vs2
            else throwError $ DoubleBound (Set.findMin vsi) p
        disjointUnion :: Set LabelName -> Set LabelName -> TIM (Set LabelName)
        disjointUnion ls1 ls2 = do -- TIM
          let lsi = Set.intersection ls1 ls2
          if Set.null lsi
            then return $ Set.union ls1 ls2
            else throwError $ DoubleLabel (Set.findMin lsi) p
        tPatCheckPrimary :: A.PrimaryPattern
                         -> TIM (Set Ident, Set LabelName)
        tPatCheckPrimary pat =
          case pat of
            A.PatPrim _ -> return (Set.empty, Set.empty)
            A.PatLabel lbl i' pp' ->
              (,) <$> (disjointVarsetUnion (Set.singleton i') =<<
                       fst <$> tPatCheckPrimary pp')
                  <*> (pure $ Set.singleton lbl)
            A.PatOnion pps -> do
              (vs, ls) <- unzip <$> mapM tPatCheckPrimary pps
              v <- foldM disjointVarsetUnion Set.empty vs
              l <- foldM disjointUnion       Set.empty ls
              return (v, l)
            A.PatFun -> return (Set.empty, Set.empty)

-- TODO: handle program points differently.
-- TODO: specify the monad more precisely than TIM; this function never
-- updates the global constraint set.
tPatType :: A.Pattern -> TIM (Gamma, Constraints, T.PatternType)
tPatType (A.Pattern i pp) = do
  ac <- freshVar
  ai <- freshVar
  (gamma, ppt) <- tPatPriType pp
-- Since the pattern has already been verified as well-formed, this is
-- unnecessary.
--  gamma' <- if Map.member i gamma
--            then throwError $ DoubleBound i pat
--            else return $ Map.insert i ac
  return ( Map.insert i ac gamma
         , Set.fromList [Cell ai <: ac .: histFIXME, Final ai .: histFIXME]
         , T.Pattern ai ppt)

tPatPriType :: A.PrimaryPattern -> TIM (Gamma, T.PrimaryPatternType)
tPatPriType pat =
  case pat of
    A.PatPrim tprim -> return (Map.empty, T.PatPrim tprim)
    A.PatLabel lbl i pp -> do
      ac <- freshVar
      (gamma, ppt) <- tPatPriType pp
      return (Map.insert i ac gamma, T.PatLabel lbl ac ppt)
    A.PatOnion pps ->
      (Map.unions *** T.PatOnion) . unzip <$> mapM tPatPriType pps
    A.PatFun -> return (Map.empty, T.PatFun)

---- |Analyzes a branch pattern and produces an appropriate type for it.
--tDigestBranch :: A.Chi a -> TIM (Gamma, Constraints, T.TauChi a)
--tDigestBranch chi =
--  case chi of
--    --TODO: Do we want binders to be final? Not making them final yet.
--    A.ChiTopVar x -> do
--      ai <- freshVar
--      ac <- freshVar
--      let c = Set.singleton (T.Cell ai <: ac .: T.histFIXME)
--      return (Map.singleton x ac, c, T.TauChiTopVar ai)
--    A.ChiTopOnion p s -> do
--      (g1,c1,tp) <- tDigestBranch p
--      (g2,c2,ts) <- tDigestBranch s
--      return (Map.union g1 g2, Set.union c1 c2, T.TauChiTopOnion tp ts)
--    A.ChiTopBind b -> do
--      (g,c,tb) <- tDigestBranch b
--      return (g, c, T.TauChiTopBind tb)
--    A.ChiOnionMany p s -> do
--      (g1,c1,tp) <- tDigestBranch p
--      (g2,c2,ts) <- tDigestBranch s
--      return (Map.union g1 g2, Set.union c1 c2, T.TauChiOnionMany tp ts)
--    A.ChiOnionOne p -> do
--      (g,c,tp) <- tDigestBranch p
--      return (g, c, T.TauChiOnionOne tp)
--    A.ChiBound x b -> do
--      (g,c,tb) <- tDigestBranch b
--      ai <- freshVar
--      ac <- freshVar
--      let c' = Set.insert (T.Cell ai <: ac .: T.histFIXME) c
--      return (Map.insert x ac g, c', T.TauChiBound ai tb)
--    A.ChiUnbound p -> do
--      (g,c,tp) <- tDigestBranch p
--      return (g, c, T.TauChiUnbound tp)
--    A.ChiPrim p -> return (Map.empty, Set.empty, T.TauChiPrim p)
--    A.ChiLabelShallow lbl x -> do
--      a <- freshVar
--      return (Map.singleton x a, Set.empty, T.TauChiLabelShallow lbl a)
--    A.ChiLabelDeep lbl b -> do
--      (g,c,tb) <- tDigestBranch b
--      return (g, c, T.TauChiLabelDeep lbl tb)
--    A.ChiFun -> return (Map.empty, Set.empty, T.TauChiFun)
--    A.ChiInnerStruct s -> do
--      (g,c,ts) <- tDigestBranch s
--      return (g, c, T.TauChiInnerStruct ts)
