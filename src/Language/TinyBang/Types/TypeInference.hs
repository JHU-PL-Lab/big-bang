module Language.TinyBang.Types.TypeInference
( inferType
, runTIM
, inferTypeTop
, Gamma
, TypeInferenceError (..)
) where

import Control.Monad (liftM, mapAndUnzipM, zipWithM)
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

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.Render.Display
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.Types ( (<:)
                                     , (.:)
                                     , Constraint(..)
                                     , TauDown(..)
                                     , Alpha(..)
                                     , Constraints
                                     )
import Language.TinyBang.Types.UtilTypes
import Language.TinyBang.Interpreter.Interpreter (applyBuiltins)

type Gamma = Map Ident Alpha
type NextFreshVar = T.AlphaId

histFIXME :: T.ConstraintHistory
histFIXME = undefined

-- |An error type for the type inference routine.
data TypeInferenceError =
    -- |Indicates that an expression contained an unbound variable.
      NotClosed Ident
      -- TODO: Add NotImplemented
    deriving (Show)
instance Error TypeInferenceError where
    strMsg = error
instance Display TypeInferenceError where
    makeDoc err = case err of
        NotClosed i -> text "not closed:" <+> (text $ unIdent i)

-- |A type alias for the type information monad.
type TIM a = ErrorT TypeInferenceError
                    (RWS Gamma Constraints NextFreshVar)
                    a

runTIM :: TIM a -> Gamma -> NextFreshVar
       -> (Either TypeInferenceError a, T.Constraints)
runTIM t r s = evalRWS (runErrorT t) r s

inferTypeTop :: A.Expr
             -> ( Either TypeInferenceError Alpha
                , Constraints
                )
inferTypeTop expr = runTIM (inferType $ applyBuiltins expr) Map.empty 0

-- |Performs type inference for a given Big Bang expression.
inferType :: A.Expr -> TIM Alpha
inferType expr =
  case expr of
    A.Var x ->
      maybe (throwError $ NotClosed x) return =<< (asks $ Map.lookup x)
    A.Label n e -> do
      a1 <- freshVar
      a2 <- inferType e
      tell1 $ TdLabel n a2 <: a1 .: histFIXME
      return a1
    A.Onion e1 e2 -> do
      a0 <- freshVar
      a1 <- inferType e1
      a2 <- inferType e2
      tell1 $ T.TdOnion a1 a2 <: a0 .: histFIXME
      return a0
    A.Func i e -> do
      a1 <- freshVar
      a2 <- freshVar
      (a3, constraints) <- capture (Map.insert i a2) e
      gamma <- ask
      vars <-
        return $ Set.difference
          ( Set.union (extractConstraintTypeVars constraints)
          $ Set.fromList [a2, a3])
          ( Set.fromList $ Map.elems gamma)
      let funcType = T.TdFunc
            (T.PolyFuncData vars a2 a3 constraints)
      tell1 $ funcType <: a1 .: T.Inferred expr gamma
      return a1
    A.Appl e1 e2 -> do
      ai <- freshVar
      ao <- freshVar
      a1 <- inferType e1
      a2 <- inferType e2
      gamma <- ask
      tell1 $ a1 <: T.TuFunc ai ao .: T.Inferred expr gamma
      tell1 $ a2 <: ai .: T.Inferred expr gamma
      return ao
    A.PrimInt _ -> do
      a <- freshVar
      tell1 $ T.PrimInt <: a .: histFIXME
      return a
    A.PrimChar _ -> do
      a <- freshVar
      tell1 $ T.PrimChar <: a .: histFIXME
      return a
    A.PrimUnit -> do
      a <- freshVar
      tell1 $ T.PrimUnit <: a .: histFIXME
      return a
    A.Case e brs -> do
      a1 <- freshVar
      a2 <- freshVar
      a2' <- inferType e
      gamma <- ask
      (brAssump, tauChis) <-
            mapAndUnzipM extractBranchAssumptionAndChi brs
      let fs = map Map.union brAssump
      (alphas, constraints) <- liftM unzip $ zipWithM capture fs $
                                 map (\(A.Branch _ _ x) -> x) brs
      tell1 $ T.Case a2'
            (zipWith3 (buildGuard expr gamma a1) tauChis constraints alphas)
            $ T.Inferred expr gamma
      return a1
    A.OnionSub e s -> do
      a1 <- freshVar
      a2 <- inferType e
      tell1 $ T.TdOnionSub a2 s <: a1 .: histFIXME
      return a1
    A.LazyOp e1 op e2 -> do
      a0 <- freshVar
      a1 <- inferType e1
      a2 <- inferType e2
      tell1 $ T.TdLazyOp op a1 a2 <: a0 .: histFIXME
      return a0
    A.EagerOp e1 op e2 -> error "Eager operations are not implemented yet"
        -- do
        --   t1 <- inferType e1
        --   t2 <- inferType e2
        --   return $ T.TdEagerOp t1 op t2
--    A.Equal e1 e2 -> error "Equality is not implemented yet"
          -- alpha <- freshVar
          -- alpha' <- freshVar
          -- gamma <- ask
          -- tell $ Set.fromList $ map (.: T.Inferred expr gamma) $
          --       map (<: T.TucAlpha alpha') $ map
          --       (\x -> T.TdcLabel (labelName x) $ T.TdcPrim T.PrimUnit)
          --       ["True","False"]
          -- naryOp expr gamma [e1,e2] (T.TucAlpha alpha) (T.TdcAlpha alpha')
      -- TODO: Make these definitions do something
    A.Def _ _ _ -> throwError $ NotClosed (ident "x")
    A.Assign _ _ _ -> throwError $ NotClosed (ident "x")
    where tell1 :: T.Constraint -> TIM ()
          tell1 = tell . Set.singleton
          -- |Infers the type of the subexpression in an environment modified by
          --  @f@, and prevents constraints from that inference from bubbling
          --  up.
          capture :: (Gamma -> Gamma) -> A.Expr -> TIM (Alpha, Constraints)
          capture f e = censor (const mempty) $ listen $ local f $ inferType e
          buildGuard expr' gamma supAlpha tauChi constraints subAlpha =
            T.Guard tauChi $
              Set.insert
                (subAlpha <: supAlpha .: T.Inferred expr' gamma)
                constraints
          -- naryOp expr' gamma es tIn tOut = do
          --   ts <- mapM inferType es
          --   tell $ Set.fromList $
          --     map (.: T.Inferred expr gamma) $
          --     map (<: tIn) ts
          --   alpha <- freshVar
          --   tell1 $ tOut <: T.TuAlpha alpha .: T.Inferred expr' gamma
          --   return alpha

-- |Accepts a branch and the case expression type and produces an appropriate
--  assumption for typechecking the corresponding branch expression. This
--  corresponds to the kappa function in the document.
extractBranchAssumptionAndChi
    :: A.Branch
    -> TIM (Gamma, T.TauChi)
extractBranchAssumptionAndChi (A.Branch _ chi _) =
  case chi of
    A.ChiPrim p -> return (Map.empty, T.ChiPrim p)
    A.ChiLabel n i -> do
      alpha <- freshVar
      return (Map.singleton i alpha, T.ChiLabel n alpha)
    A.ChiFun -> return (Map.empty, T.ChiFun)
    A.ChiAny -> return (Map.empty, T.ChiAny)

-- |Extracts all type variables from the provided constraints.
extractConstraintTypeVars :: T.Constraints -> Set T.Alpha
extractConstraintTypeVars c =
    Foldable.foldl foldConstraints Set.empty c
    where foldConstraints set el =
            case el of
                LowerSubtype _ a _ ->
                  Set.insert a set
                UpperSubtype a _ _ ->
                  Set.insert a set
                AlphaSubtype a1 a2 _ ->
                  Set.union set $ Set.fromList [a1, a2]
                T.Case alpha guards _ ->
                    let set' = Set.insert alpha set in
                    foldl foldGuards set' guards
                T.Bottom _ -> set
          foldGuards set (T.Guard tauChi constraints) =
            Set.union set $ addChiAlpha tauChi $
                extractConstraintTypeVars constraints
          addChiAlpha tauChi set =
            case tauChi of
                T.ChiPrim _ -> set
                T.ChiLabel _ a -> Set.insert a set
                T.ChiFun -> set
                T.ChiAny -> set

-- |Creates a fresh type variable for the type inference engine.
freshVar :: TIM T.Alpha
freshVar = do
    idx <- get
    put $ idx + 1
    return $ T.Alpha idx $ T.callSites []
