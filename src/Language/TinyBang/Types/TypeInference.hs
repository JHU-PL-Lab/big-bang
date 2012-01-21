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
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Maybe (catMaybes)

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.Render.Display
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.Types ((<:), (.:))
import Language.TinyBang.Types.UtilTypes
import Language.TinyBang.Interpreter.Interpreter (applyBuiltins)

type Gamma = Map Ident T.Alpha
type NextFreshVar = T.AlphaId

-- |An error type for the type inference routine.
data TypeInferenceError =
    -- |Indicates that an expression contained an unbound variable.
      NotClosed Ident
    deriving (Show)
instance Error TypeInferenceError where
    strMsg = error
instance Display TypeInferenceError where
    makeDoc err = case err of
        NotClosed i -> text "not closed:" <+> (text $ unIdent i)

-- |A type alias for the type information monad.
type TIM a = ErrorT TypeInferenceError
                    (RWS Gamma T.Constraints NextFreshVar)
                    a

runTIM :: TIM a -> Gamma -> NextFreshVar
       -> (Either TypeInferenceError a, T.Constraints)
runTIM t r s = evalRWS (runErrorT t) r s

inferTypeTop :: A.Expr
             -> ( Either TypeInferenceError T.TauDown
                , T.Constraints
                )
inferTypeTop expr = runTIM (inferType $ applyBuiltins expr) Map.empty 0

-- |Performs type inference for a given Big Bang expression.
inferType :: A.Expr -> TIM T.TauDown
inferType expr =
    case expr of
        A.Var x ->
            maybe (throwError $ NotClosed x) (return . T.TdAlpha) =<<
                    (asks $ Map.lookup x)
        A.Label n e ->
            return . T.TdLabel n . T.TdAlpha =<< inferTypeOverFreshInter e
        A.Onion e1 e2 ->
            do
              a1 <- liftM T.TdAlpha $ inferTypeOverFreshInter e1
              a2 <- liftM T.TdAlpha $ inferTypeOverFreshInter e2
              return $ T.TdOnion a1 a2
        A.Func i e ->
            do
              alpha1 <- freshVar
              alpha2 <- freshVar
              alpha3 <- freshVar
              (tau, constraints) <- capture (Map.insert i alpha3) e
              gamma <- ask
              vars <- do
                    return $
                        (
                            Set.union (extractConstraintTypeVars constraints) $
                            Set.fromList [alpha2, alpha3]
                        )
                        \\ -- set subtraction
                        (Set.fromList $ Map.elems gamma)
              let constraints' =
                    Set.insert (tau <: T.TuAlpha alpha2 .: T.Inferred expr gamma)
                               constraints
              let funcType = T.TdFunc
                    (T.PolyFuncData vars alpha3 alpha2 constraints')
              tell1 $ funcType <: T.TuAlpha alpha1 .: T.Inferred expr gamma
              ralpha alpha1
        A.Appl e1 e2 ->
            do
              alphaIn <- freshVar
              alphaOut <- freshVar
              t1 <- inferType e1
              t2 <- inferType e2
              gamma <- ask
              tell1 $ t1 <: T.TuFunc alphaIn alphaOut .: T.Inferred expr gamma
              tell1 $ t2 <: T.TuAlpha alphaIn .: T.Inferred expr gamma
              ralpha alphaOut
        A.PrimInt _ -> rprim T.PrimInt
        A.PrimChar _ -> rprim T.PrimChar
        A.PrimUnit -> rprim T.PrimUnit
        A.Case e brs ->
            do
              alpha1 <- freshVar
              alpha2 <- freshVar
              t <- inferType e
              gamma <- ask
              (brAssump, tauChis) <-
                    mapAndUnzipM extractBranchAssumptionAndChi brs
              let fs = map Map.union brAssump
              (taus, constraints) <- liftM unzip $ zipWithM capture fs $
                                         map (\(A.Branch _ _ x) -> x) brs
              tell1 $ t <: T.TuAlpha alpha2 .: T.Inferred expr gamma
              tell1 $ T.Case alpha2
                    (zipWith3 (buildGuard expr gamma alpha1) tauChis constraints taus)
                    $ T.Inferred expr gamma
              ralpha alpha1
        A.LazyOp e1 op e2 ->
            do
              t1 <- inferType e1
              t2 <- inferType e2
              return $ T.TdLazyOp t1 op t2
        A.EagerOp e1 op e2 -> error "Eager operations are not implemented yet"
            -- do
            --   t1 <- inferType e1
            --   t2 <- inferType e2
            --   return $ T.TdEagerOp t1 op t2
--        A.Equal e1 e2 -> error "Equality is not implemented yet"
              -- alpha <- freshVar
              -- alpha' <- freshVar
              -- gamma <- ask
              -- tell $ Set.fromList $ map (.: T.Inferred expr gamma) $
              --       map (<: T.TucAlpha alpha') $ map
              --       (\x -> T.TdcLabel (labelName x) $ T.TdcPrim T.PrimUnit)
              --       ["True","False"]
              -- naryOp expr gamma [e1,e2] (T.TucAlpha alpha) (T.TdcAlpha alpha')
    where rprim = return . T.TdPrim
          ralpha = return . T.TdAlpha
          tell1 :: T.Constraint -> TIM ()
          tell1 = tell . Set.singleton
          capture f e = censor (const mempty) $ listen $
                local f $ inferType e
          buildGuard expr' gamma alpha tauChi constraints tau =
                T.Guard tauChi $
                        Set.insert
                          (tau <: T.TuAlpha alpha .: T.Inferred expr' gamma)
                          constraints
          naryOp expr' gamma es tIn tOut = do
                ts <- mapM inferType es
                tell $ Set.fromList $
                  map (.: T.Inferred expr gamma) $
                  map (<: tIn) ts
                alpha <- freshVar
                tell1 $ tOut <: T.TuAlpha alpha .: T.Inferred expr' gamma
                ralpha alpha

-- |Accepts a branch and the case expression type and produces an appropriate
--  assumption for typechecking the corresponding branch expression.
extractBranchAssumptionAndChi
    :: A.Branch
    -> TIM (Map Ident T.Alpha, T.TauChi)
extractBranchAssumptionAndChi (A.Branch _ chi _) =
    case chi of
        A.ChiPrim p -> return (Map.empty, T.ChiPrim p)
        A.ChiLabel n i -> do
            alpha <- freshVar
            return (Map.singleton i alpha, T.ChiLabel n alpha)
        A.ChiFun -> return (Map.empty, T.ChiFun)
        A.ChiAny -> return (Map.empty, T.ChiAny)

alphaFromTd :: T.TauDown -> Maybe T.Alpha
alphaFromTd t =
  case t of
    T.TdAlpha a -> Just a
    _ -> Nothing

alphaFromTu :: T.TauUp -> Maybe T.Alpha
alphaFromTu t =
  case t of
    T.TuAlpha a -> Just a
    _ -> Nothing

-- |Extracts all type variables from the provided constraints.
extractConstraintTypeVars :: T.Constraints -> Set T.Alpha
extractConstraintTypeVars c =
    Foldable.foldl foldConstraints Set.empty c
    where foldConstraints set el =
            case el of
                T.Subtype td tu _ ->
                    foldr Set.insert set (catMaybes [alphaFromTd td, alphaFromTu tu])
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

-- |Given an expression, infers a type for that expression, creates a new type
--  variable, adds the constraint that the type variable is a supertype of the
--  expression's inferred type, and returns the type variable.
inferTypeOverFreshInter :: A.Expr -> TIM T.Alpha
inferTypeOverFreshInter e = do
    tau <- inferType e
    alpha <- freshVar
    gamma <- ask
    tell $ Set.singleton $ tau <: T.TuAlpha alpha .: T.Inferred e gamma
    return $ alpha

-- |Creates a fresh type variable for the type inference engine.
freshVar :: TIM T.Alpha
freshVar = do
    idx <- get
    put $ idx + 1
    return $ T.Alpha idx $ T.callSites []