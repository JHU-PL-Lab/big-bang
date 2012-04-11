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
import Control.Applicative ((<$>))

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
                                     )
import Language.TinyBang.Types.UtilTypes
import Language.TinyBang.Interpreter.Interpreter (applyBuiltins)
import Utils.Render.Display

type Gamma = Map Ident CellAlpha
type NextFreshVar = T.AlphaId

-- |An error type for the type inference routine.
data TypeInferenceError
      -- |Indicates that an expression contained an unbound variable.
      = NotClosed Ident
      -- |Indicates that a variable is bound twice in a pattern
      | DoubleBound Ident A.ChiMain
      -- |Indicates that a label name is used twice in a multipattern
      | DoubleLabel LabelName A.ChiMain
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
  runTIM (inferType $ applyBuiltins expr) Map.empty 0

-- |Performs type inference for a given Big Bang expression.
inferType :: A.Expr -> TIM InterAlpha
inferType expr = do
  gamma <- ask
  let inferred = T.Inferred expr gamma
      tellInferred c = do
        tell1 $ c .: inferred
  case expr of
    A.Var x -> do
      a2 <- maybe (throwError $ NotClosed x) return =<< (asks $ Map.lookup x)
      a1 <- freshVar
      tellInferred $ a2 <: CellGet a1
      return a1
    A.Label n e -> do
      a1 <- freshVar
      a2 <- inferType e
      a3 <- freshVar
      tellInferred $ Cell a2 <: a3
      tellInferred $ TdLabel n a3 <: a1
      return a1
    A.Onion e1 e2 -> do
      a0 <- freshVar
      a1 <- inferType e1
      a2 <- inferType e2
      tellInferred $ TdOnion a1 a2 <: a0
      return a0
    A.Func i e -> do
      a1 <- freshVar
      a2 <- freshVar
      (a3, constraints) <- capture (Map.insert i a2) e
      vars <-
        return $ Set.difference
          ( Set.union (extractConstraintTypeVars constraints)
          $ Set.fromList [alphaWeaken a2, alphaWeaken a3])
          ( Set.fromList $ map alphaWeaken $ Map.elems gamma)
      let funcType = T.TdFunc
            (T.PolyFuncData (T.ForallVars vars) a2 a3 constraints)
      tellInferred $ funcType <: a1
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
    A.Case e branches -> do
      a1' <- freshVar
      a2' <- inferType e

      -- validate patterns for well-formedness
      mapM_ tPatCheck $ map (\(A.Branch chi _) -> chi) branches

      -- branchData contains one element for each branch to process
      -- it contains information in digested form
      branchData <- sequence $ do
        A.Branch chi branchExpr <- branches
        return $ ( (,branchExpr) <$> tDigestBranch chi)

      guards <- sequence $ do
        ((branchGamma, branchConstr, tauChi), branchExpr) <- branchData
        let mAlphaConstraints = capture (Map.union branchGamma) branchExpr
            buildGuard (an, constraints) =
              T.Guard tauChi $
                Set.insert (an <: a1' .: inferred) $
                Set.unions [constraints,branchConstr]
        return $ buildGuard <$> mAlphaConstraints

      tell1 $ (\bs -> Case a2' bs $ T.Inferred e gamma) guards
      return a1'
    A.OnionSub e s -> do
      a1 <- freshVar
      a2 <- inferType e
      tellInferred $ TdOnionSub a2 s <: a1
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
    A.Def x e1 e2 -> do
      --TODO: Something about shadowing
      a1 <- inferType e1
      a3 <- freshVar
      a2 <- local (Map.insert x a3) $ inferType e2
      tellInferred $ Cell a1 <: a3
      return a2
    A.Assign a e1 e2 -> do
      x <- return $! case a of
        A.AIdent x -> x
        A.ACell _ -> error "Internal Error; assignment to cell during type derivation!"
      a3 <- maybe (throwError $ NotClosed x) return =<< (asks $ Map.lookup x)
      a1 <- inferType e1
      a2 <- inferType e2
      tellInferred $ a3 <: CellSet a1
      return a2
    A.ExprCell _ -> error "Implementation error; infer called on ExprCell"
    where tell1 :: T.Constraint -> TIM ()
          tell1 = tell . Set.singleton
          -- |Infers the type of the subexpression in an environment modified by
          --  @f@, and prevents constraints from that inference from bubbling
          --  up.
          capture :: (Gamma -> Gamma) -> A.Expr -> TIM (InterAlpha, Constraints)
          capture f e = censor (const mempty) $ listen $ local f $ inferType e

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
                Equivalent {} -> set
                Comparable a1 a2 _ -> insert2Weak set a1 a2
                Final a _ -> insertWeak set a
                Immutable a _ -> insertWeak set a
                T.Case a gs _ ->
                    let set' = insertWeak set a in
                    foldl foldGuards set' gs
                T.Bottom _ -> set
          foldTau set tau =
            case tau of
              TdPrim _ -> set
              TdLabel _ a -> insertWeak set a
              TdOnion a1 a2 -> insert2Weak set a1 a2
              TdFunc (T.PolyFuncData (T.ForallVars as) ca ia cs) ->
                Set.union set $
                Set.difference
                  (insert2Weak (extractConstraintTypeVars cs) ca ia)
                  as
              TdOnionSub a _ -> insertWeak set a
              TdEmptyOnion -> set
          foldGuards set (T.Guard tauChi constraints) =
            Set.unions [set, chiAlphas tauChi,
                        extractConstraintTypeVars constraints]
          chiAlphas :: T.TauChi a -> Set AnyAlpha
          chiAlphas tauChi =
            case tauChi of
              T.TauChiTopVar a -> Set.singleton $ alphaWeaken a
              T.TauChiTopOnion p s -> Set.union (chiAlphas p) (chiAlphas s)
              T.TauChiTopBind b -> chiAlphas b
              T.TauChiOnionMany p s -> Set.union (chiAlphas p) (chiAlphas s)
              T.TauChiOnionOne p -> chiAlphas p
              T.TauChiBound a b -> insertWeak (chiAlphas b) a
              T.TauChiUnbound p -> chiAlphas p
              T.TauChiPrim _ -> Set.empty
              T.TauChiLabelShallow _ a -> Set.singleton $ alphaWeaken a
              T.TauChiLabelDeep _ b -> chiAlphas b
              T.TauChiFun -> Set.empty
              T.TauChiInnerStruct s -> chiAlphas s
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
tPatCheck :: A.ChiMain -> TIM (Set Ident, Set LabelName)
tPatCheck chiMain = tPatCheckInner chiMain
  where onion :: A.ChiPrimary -> A.ChiStruct -> TIM (Set Ident, Set LabelName)
        onion p s = do -- TIM
          (v1,l1) <- tPatCheckInner p
          (v2,l2) <- tPatCheckInner s
          v <- disjointVarsetUnion v1 v2
          l <- disjointUnion l1 l2
          return (v,l)
        disjointVarsetUnion :: Set Ident -> Set Ident -> TIM (Set Ident)
        disjointVarsetUnion vs1 vs2 = do -- TIM
          let vsi = Set.delete (ident "_") $ Set.intersection vs1 vs2
          if Set.null vsi
            then return $ Set.union vs1 vs2
            else throwError $ DoubleBound (Set.findMin vsi) chiMain
        disjointUnion :: Set LabelName -> Set LabelName -> TIM (Set LabelName)
        disjointUnion ls1 ls2 = do -- TIM
          let lsi = Set.intersection ls1 ls2
          if Set.null lsi
            then return $ Set.union ls1 ls2
            else throwError $ DoubleLabel (Set.findMin lsi) chiMain
        tPatCheckInner :: A.Chi a
                       -> TIM (Set Ident, Set LabelName)
        tPatCheckInner chi =
          case chi of
            A.ChiTopVar x -> return (Set.singleton x, Set.empty)
            A.ChiTopOnion p s -> onion p s
            A.ChiTopBind b -> tPatCheckInner b
            A.ChiOnionMany p s -> onion p s
            A.ChiOnionOne p -> tPatCheckInner p
            A.ChiBound x b -> do -- TIM
              (v,l) <- tPatCheckInner b
              v' <- disjointVarsetUnion v $ Set.singleton x
              return (v', l)
            A.ChiUnbound p -> tPatCheckInner p
            A.ChiPrim _ -> return (Set.empty, Set.empty)
            A.ChiLabelShallow lbl x -> return (Set.singleton x, Set.singleton lbl)
            A.ChiLabelDeep lbl b -> do -- TIM
              (v,_) <- tPatCheckInner b
              return (v, Set.singleton lbl)
            A.ChiFun -> return (Set.empty, Set.empty)
            A.ChiInnerStruct s -> tPatCheckInner s

-- |Analyzes a branch pattern and produces an appropriate type for it.
tDigestBranch :: A.Chi a -> TIM (Gamma, Constraints, T.TauChi a)
tDigestBranch chi =
  case chi of
    --TODO: Do we want binders to be final? Not making them final yet.
    A.ChiTopVar x -> do
      ai <- freshVar
      ac <- freshVar
      let c = Set.singleton (T.Cell ai <: ac .: T.histFIXME)
      return (Map.singleton x ac, c, T.TauChiTopVar ai)
    A.ChiTopOnion p s -> do
      (g1,c1,tp) <- tDigestBranch p
      (g2,c2,ts) <- tDigestBranch s
      return (Map.union g1 g2, Set.union c1 c2, T.TauChiTopOnion tp ts)
    A.ChiTopBind b -> do
      (g,c,tb) <- tDigestBranch b
      return (g, c, T.TauChiTopBind tb)
    A.ChiOnionMany p s -> do
      (g1,c1,tp) <- tDigestBranch p
      (g2,c2,ts) <- tDigestBranch s
      return (Map.union g1 g2, Set.union c1 c2, T.TauChiOnionMany tp ts)
    A.ChiOnionOne p -> do
      (g,c,tp) <- tDigestBranch p
      return (g, c, T.TauChiOnionOne tp)
    A.ChiBound x b -> do
      (g,c,tb) <- tDigestBranch b
      ai <- freshVar
      ac <- freshVar
      let c' = Set.insert (T.Cell ai <: ac .: T.histFIXME) c
      return (Map.insert x ac g, c', T.TauChiBound ai tb)
    A.ChiUnbound p -> do
      (g,c,tp) <- tDigestBranch p
      return (g, c, T.TauChiUnbound tp)
    A.ChiPrim p -> return (Map.empty, Set.empty, T.TauChiPrim p)
    A.ChiLabelShallow lbl x -> do
      a <- freshVar
      return (Map.singleton x a, Set.empty, T.TauChiLabelShallow lbl a)
    A.ChiLabelDeep lbl b -> do
      (g,c,tb) <- tDigestBranch b
      return (g, c, T.TauChiLabelDeep lbl tb)
    A.ChiFun -> return (Map.empty, Set.empty, T.TauChiFun)
    A.ChiInnerStruct s -> do
      (g,c,ts) <- tDigestBranch s
      return (g, c, T.TauChiInnerStruct ts)
