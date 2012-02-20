{-# LANGUAGE Rank2Types #-}
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
      -- |Indicates that the binder and label use the same variable.
      | DoubleBound Ident
      -- TODO: Add NotImplemented
    deriving (Show)
instance Error TypeInferenceError where
    strMsg = error
instance Display TypeInferenceError where
    makeDoc err = case err of
        NotClosed i ->
          text "TypeInference: not closed:" <+>
          (text $ unIdent i)
        DoubleBound i ->
          text "TypeInference: bound both in the binder and in the label:" <+>
          (text $ unIdent i)

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
            (T.PolyFuncData vars a2 a3 constraints)
      tellInferred $ funcType <: a1
      return a1
    A.Appl e1 e2 -> do
      a1' <- freshVar
      a2' <- freshVar
      a1 <- inferType e1
      a2 <- inferType e2
      tellInferred $ a1 <: T.TuFunc a1' a2'
      tellInferred $ Cell a2 <: a1'
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
    A.Case e branches -> error "Not Implemented" {- do
      a1' <- freshVar
      a2' <- inferType e

      bundle <- sequence $ do
        A.Branch binder chi branchExpr <- branches
        return $
          (\(x,y,z) -> (x,y,z,branchExpr)) <$> tDigestBranch a2' binder chi

      guards <- sequence $ do
        (branchGamma, newConstraints, tauChi, branchExpr) <- bundle
        let mAlphaConstraints = inferForBranch branchGamma branchExpr
            buildGuard (an, constraints) =
              T.Guard tauChi $
                Set.insert (an <: a1' .: inferred) $
                Set.union constraints newConstraints
        return $ buildGuard <$> mAlphaConstraints

      tell1 $ (\bs -> Case a2' bs $ T.Inferred e gamma) guards
      return a1'
      where tDigestBranch a1 binder chi = do
              a2 <- freshVar
              (g', c) <- newGammaAndConstraints
              (\(x, y) -> (x, c, y)) <$> case chi of
                A.ChiAny ->
                  return (g', T.ChiAny)
                A.ChiPrim p ->
                  return (g', T.ChiPrim p)
                A.ChiLabel n x | Just x /= binder ->
                  return (Map.insert x a2 g' , T.ChiLabel n a2)
                A.ChiLabel _ x ->
                  throwError $ DoubleBound x
                A.ChiFun ->
                  return (g', T.ChiFun)
              where newGammaAndConstraints =
                      case binder of
                        Just x -> do
                          a3 <- freshVar
                          return
                            ( Map.singleton x a3
                            , Set.singleton $ Cell a1 <: a3 .: inferred
                            )
                        Nothing -> return mempty
            inferForBranch branchGamma branchExpr =
              capture (Map.union branchGamma) branchExpr
-}
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
          -- naryOp expr' gamma es tIn tOut = do
          --   ts <- mapM inferType es
          --   tell $ Set.fromList $
          --     map (.: T.Inferred expr gamma) $
          --     map (<: tIn) ts
          --   alpha <- freshVar
          --   tell1 $ tOut <: T.TuAlpha alpha .: T.Inferred expr' gamma
          --   return alpha

-- |Extracts all type variables from the provided constraints.
extractConstraintTypeVars :: T.Constraints -> Set AnyAlpha
extractConstraintTypeVars c =
    Foldable.foldl foldConstraints Set.empty c
    where foldConstraints set el =
            case el of
                LowerSubtype _ a _ -> insertWeak set a
                UpperSubtype a _ _ -> insertWeak set a
                AlphaSubtype a1 a2 _ -> insert2Weak set a1 a2
                CellSubtype a1 a2 _ -> insert2Weak set a1 a2
                CellGetSubtype a1 a2 _ -> insert2Weak set a1 a2
                CellSetSubtype a1 a2 _ -> insert2Weak set a1 a2
                CellAlphaSubtype a1 a2 _ -> insert2Weak set a1 a2
                LazyOpSubtype _ a1 a2 a3 _ -> insert3Weak set a1 a2 a3
                Comparable a1 a2 _ -> insert2Weak set a1 a2
                T.Case a gs _ ->
                    let set' = insertWeak set a in
                    foldl foldGuards set' gs
                T.Bottom _ -> set
          foldGuards set (T.Guard tauChi constraints) =
            Set.union set $ addChiAlpha tauChi $
                extractConstraintTypeVars constraints
          addChiAlpha tauChi set =
            case tauChi of
                T.ChiPrim _ -> set
                T.ChiLabel _ a -> insertWeak set a
                T.ChiFun -> set
                T.ChiAny -> set
          insertWeak set a = Set.insert (alphaWeaken a) set
          insert2Weak set a1 a2 =
            insertWeak (insertWeak set a1) a2
          insert3Weak set a1 a2 a3 =
            insertWeak (insert2Weak set a1 a2) a3

-- |Creates a fresh type variable for the type inference engine.
freshVar :: (AlphaType a) => TIM (SomeAlpha a)
freshVar = do
    idx <- get
    put $ idx + 1
    return $ T.makeNewAlpha idx
