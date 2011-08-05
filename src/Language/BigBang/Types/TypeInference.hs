module Language.BigBang.Types.TypeInference
( inferType
, runTIM
) where

import Control.Monad (liftM, mapM, mapAndUnzipM, zipWithM)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.Writer (censor, listen, tell)
import Control.Monad.State (get, put)
import Control.Monad.RWS (RWS, evalRWS)
import Control.Monad.Error (Error, ErrorT, strMsg, throwError, runErrorT)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (maybe)
import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Set as Set
import Data.Set (Set, (\\))

import qualified Language.BigBang.Ast as A
import Language.BigBang.Render.Display
import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Types.Types ((<:))
import Language.BigBang.Types.UtilTypes

type Gamma = Map Ident T.AlphaUp
type InferredConstraints = Set T.Constraint
type NextFreshVar = Integer

-- |An error type for the type inference routine.
data TypeInferenceError =
    -- |Indicates that an expression contained an unbound variable.
      NotClosed Ident
    deriving (Show)
instance Error TypeInferenceError where
    strMsg = error

-- |A type alias for the type information monad.
type TIM a = ErrorT TypeInferenceError
                    (RWS Gamma InferredConstraints NextFreshVar)
                    a

runTIM :: TIM a -> Gamma -> NextFreshVar
       -> (Either TypeInferenceError a, InferredConstraints)
runTIM t r s = evalRWS (runErrorT t) r s

-- |Performs type inference for a given Big Bang expression.
inferType :: A.Expr -> TIM T.TauDownClosed
inferType e =
    case e of
        A.Var x ->
            maybe (throwError $ NotClosed x) (return . T.TdcAlphaUp) =<<
                    (asks $ Map.lookup x)
        A.Label n e ->
            return . T.TdcLabel n . T.TdcAlpha =<< inferTypeOverFreshInter e
        A.Onion e1 e2 ->
            do
              a1 <- liftM T.TdcAlpha $ inferTypeOverFreshInter e1
              a2 <- liftM T.TdcAlpha $ inferTypeOverFreshInter e2
              return $ T.TdcOnion a1 a2
        A.Func i e ->
            do
              alpha <- freshInterVar
              alpha' <- freshInterVar
              alphaUp <- freshUpperVar
              (tau, constraints) <- capture (Map.insert i alphaUp) e
              vars <- do
                    dict <- ask
                    return $
                        (
                            Set.union (extractConstraintTypeVars constraints) $
                            Set.fromList [T.SomeAlpha alpha'
                                         ,T.SomeAlphaUp alphaUp]
                        )
                        \\ -- set subtraction
                        (Set.fromList $ map T.SomeAlphaUp (Map.elems dict))
              let constraints' =
                    Set.insert (tau <: T.TucAlpha alpha') constraints
              let funcType = T.TdcFunc
                    (T.PolyFuncData vars alphaUp alpha' constraints')
              tell1 $ funcType <: T.TucAlpha alpha
              ralpha alpha
        A.Appl e1 e2 ->
            do
              alphaUp <- freshUpperVar
              alpha <- freshInterVar
              t1 <- inferType e1
              t2 <- inferType e2
              tell1 $ t1 <: T.TucFunc alphaUp alpha
              tell1 $ t2 <: T.TucAlphaUp alphaUp
              ralpha alpha
        A.PrimInt _ -> rprim T.PrimInt
        A.PrimChar _ -> rprim T.PrimChar
        A.PrimUnit -> rprim T.PrimUnit
        A.Case e brs ->
            do
              alpha <- freshInterVar
              alphaUp <- freshUpperVar
              t <- inferType e
              gamma <- ask
              (brAssump, tauChis) <-
                    mapAndUnzipM extractBranchAssumptionAndChi brs
              let fs = map Map.union brAssump
              (taus, constraints) <- liftM unzip $ zipWithM capture fs $
                                         map snd brs
              tell1 $ t <: T.TucAlphaUp alphaUp
              tell1 $ T.Case alphaUp $
                    zipWith3 (buildGuard alpha) tauChis constraints taus
              ralpha alpha
        A.Plus e1 e2 ->
              naryOp [e1,e2] (T.TucPrim T.PrimInt) (T.TdcPrim T.PrimInt)
        A.Minus e1 e2 ->
              naryOp [e1,e2] (T.TucPrim T.PrimInt) (T.TdcPrim T.PrimInt)
        A.Equal e1 e2 -> do
              alpha <- freshInterVar
              alpha' <- freshInterVar
              tell $ Set.fromList $ map (<: T.TucAlpha alpha') $ map
                    (\x -> T.TdcLabel (labelName x) $ T.TdcPrim T.PrimUnit)
                    ["True","False"]
              naryOp [e1,e2] (T.TucAlpha alpha) (T.TdcAlpha alpha')
    where rprim = return . T.TdcPrim
          ralpha = return . T.TdcAlpha
          tell1 = tell . Set.singleton
          capture f e = censor (const mempty) $ listen $
                local f $ inferType e
          buildGuard alpha tauChi constraints tau =
                T.Guard tauChi $
                        Set.insert (tau <: T.TucAlpha alpha) constraints
          naryOp es tIn tOut = do
                ts <- mapM inferType es
                tell $ Set.fromList $ map (<: tIn) ts
                alpha <- freshInterVar
                tell1 $ tOut <: T.TucAlpha alpha
                ralpha alpha

-- |Accepts a branch and the case expression type and produces an appropriate
--  assumption for typechecking the corresponding branch expression.
extractBranchAssumptionAndChi
    :: A.Branch
    -> TIM (Map Ident T.AlphaUp, T.TauChi)
extractBranchAssumptionAndChi (chi,_) =
    case chi of
        A.ChiPrim p -> return (Map.empty, T.ChiPrim p)
        A.ChiLabel n i -> do
            alphaUp <- freshUpperVar
            return (Map.singleton i alphaUp, T.ChiLabel n alphaUp)
        A.ChiFun -> return (Map.empty, T.ChiFun)
        A.ChiTop -> return (Map.empty, T.ChiTop)
        
-- |Inserts the contents of Just into a set (or ignores Nothing).
maybeInsert :: (Ord a) => Maybe a -> Set a -> Set a
maybeInsert v set = maybe id Set.insert v $ set

-- |Extracts all type variables from the provided constraints.
extractConstraintTypeVars :: InferredConstraints -> Set T.AnyAlpha
extractConstraintTypeVars c =
    Foldable.foldl foldConstraints Set.empty c
    where foldConstraints set el =
            case el of
                T.Subtype tdc1 tdc2 ->
                    maybeInsert (T.toSomeAlpha tdc1) $
                        maybeInsert (T.toSomeAlpha tdc2) set
                T.Case alphaUp guards ->
                    let set' = Set.insert (T.SomeAlphaUp alphaUp) set in
                    foldl foldGuards set' guards
                T.Bottom -> set
          foldGuards set (T.Guard tauChi constraints) =
            Set.union set $ addChiAlpha tauChi $
                extractConstraintTypeVars constraints
          addChiAlpha tauChi set =
            case tauChi of
                T.ChiPrim p -> set
                T.ChiLabel n a -> Set.insert (T.SomeAlphaUp a) set
                T.ChiFun -> set
                T.ChiTop -> set

-- |Given an expression, infers a type for that expression, creates a new type
--  variable, adds the constraint that the type variable is a supertype of the
--  expression's inferred type, and returns the type variable.
inferTypeOverFreshInter :: A.Expr -> TIM T.Alpha
inferTypeOverFreshInter e = do
    tau <- inferType e
    alpha <- freshInterVar
    tell $ Set.singleton $ tau <: T.TucAlpha alpha
    return $ alpha

-- |Creates a fresh intermediate type variable for the type inference engine.
freshInterVar :: TIM T.Alpha
freshInterVar = freshVar T.Alpha

-- |Creates a fresh upper type variable for the type inference engine.
freshUpperVar :: TIM T.AlphaUp
freshUpperVar = freshVar T.AlphaUp

-- |Creates a fresh type variable for the type inference engine.
freshVar :: (T.AlphaContents -> a) -> TIM a
freshVar constr = do
    idx <- get
    put $ idx + 1
    return $ constr $ T.AlphaContents idx $ T.callSites []


