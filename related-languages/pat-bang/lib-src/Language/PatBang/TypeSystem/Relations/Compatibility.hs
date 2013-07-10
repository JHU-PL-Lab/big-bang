{-# LANGUAGE TupleSections, ScopedTypeVariables, TemplateHaskell #-} 

module Language.PatBang.TypeSystem.Relations.Compatibility
( CompatM
, CompatibilityError(..)
, checkApplicationCompatible
, checkTopCompatible
, checkCompatible
, Bindings
, PossibleBindings
) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast as A
import Language.PatBang.Display
import Language.PatBang.Logging
import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Fibrations
import Language.PatBang.TypeSystem.Monad.Trans.Flow
import Language.PatBang.TypeSystem.Monad.Trans.CReader
import Language.PatBang.TypeSystem.Relations.Patterns
import Language.PatBang.TypeSystem.Relations.Projection
import Language.PatBang.TypeSystem.Types as T

$(loggingFunctions)

type CompatM db m = FlowT (EitherT (CompatibilityError db) m)
type Bindings db = Map PatTVar (Type db)
type PossibleBindings db = Maybe (Bindings db)
type Visits = Set T.PatternBody
type ContractiveOccurrenceCheck = Set (FlowTVar, T.PatternBody)

data CompatibilityError db
  = CompatibilityProjectionError (ProjectionError db)
  | NonContractivePattern T.PatternBody
  deriving (Eq, Ord, Show)
  
instance (ConstraintDatabase db, Display db)
      => Display (CompatibilityError db) where
  makeDoc err = case err of
    CompatibilityProjectionError err' -> makeDoc err'
    NonContractivePattern tpat ->
      text "NonContractivePattern" <+> parens (makeDoc tpat)

checkApplicationCompatible :: forall db m.
                              ( Applicative m, ConstraintDatabase db
                              , MonadCReader db m, Display db)
                           => FlowTVar
                           -> [Type db]
                           -> CompatM db m (Maybe (FlowTVar, db), Fibration db)
checkApplicationCompatible a1 ts =
  _debugI ("Checking application compatibility for " ++ display a1 ++ " with "
              ++ display ts) $
  do
    (result,fib) <- doCheck Unexpanded ts
    _debug $ "Application compatibility for " ++ display a1 ++ " with "
        ++ display ts ++ " at fibration " ++ display fib ++
          case result of
            Nothing -> " can fail"
            Just (a,cs) -> " can succeed with " ++ display a ++ "\\"
                              ++ display cs
    return (result,fib)
  where
    doCheck :: Fibration db
            -> [Type db]
            -> CompatM db m (Maybe (FlowTVar, db), Fibration db)
    doCheck fib scapes =
      case scapes of
        [] ->
          _debugI ("Application compatibility check for argument "
                      ++ display a1 ++ " exhausted with fibration "
                      ++ display fib) $
          return (Nothing, fib)
        scape@(Scape a2 a3) : scapes' -> do
          _debug $ "Checking application compatibility for " ++ display a1
                      ++ " with " ++ display scape
          -- Get the individual pieces of the scape
          (patParts,_) <- liftCompat $ project projPat a2 Unexpanded
          (funParts,_) <- liftCompat $ project projFun a3 Unexpanded
          if null funParts || null patParts
            then return (Nothing, fib)
            else do
              let (pvars,tpat):_ = patParts
              let (tvars,rtvar,db):_ = funParts
              -- See if the argument matches
              (mtyps, fib') <- checkTopCompatible a1 (pvars,tpat) fib
              let debugPfx = "Application compatibility for " ++ display a1
                    ++ " with " ++ display scape ++ " at fibration "
                    ++ display fib'
              case mtyps of
                Nothing ->
                  _debugI (debugPfx ++ " can fail; moving on...") $
                  doCheck fib' scapes'
                Just typs ->
                  if length typs < length tvars
                    then _debugI (debugPfx ++
                      " does not bind enough arguments; moving on...") $
                      doCheck fib' scapes'
                    else do
                      let histList = CompatibilityWiring : histList
                      let cs = map cwrap $ zipWith TypeConstraint typs tvars
                      let newdb = db `union` fromList (zip cs histList)
                      return (Just (rtvar, newdb), fib')
        t:_ -> error $ "Called checkApplicationCompatible with list "
                    ++ "containing non-scape type " ++ display t

-- |Performs a compatibility check.  This function accepts an argument type,
--  a pattern, and a filtering fibration.  It performs non-deterministic
--  disjunctive computation to discover possible type lists and the fibrations
--  which generate them; all such fibrations will be subsumed by the filtering
--  fibration.
checkTopCompatible :: forall db m.
                      ( Applicative m, ConstraintDatabase db, MonadCReader db m
                      , Display db)
                   => FlowTVar
                   -> ([PatTVar], T.PatternBody)
                   -> Fibration db
                   -> CompatM db m (Maybe [Type db], Fibration db)
checkTopCompatible a1 (pvars, tpat) ffib = do
  (pbinds, fib) <- checkCompatible a1 tpat Set.empty Set.empty ffib
  case pbinds of
    Nothing -> return (Nothing, fib)
    Just binds -> return (Just $ gather pvars binds, fib)
  where
    gather :: [PatTVar] -> Map PatTVar (Type db) -> [Type db]
    gather pvs m = case pvs of
      [] -> []
      pv:pvs' -> case Map.lookup pv m of
                    Nothing -> []
                    Just t -> t : gather pvs' m

-- |Performs a compatibility check.  This function accepts an argument type,
--  a pattern body, a non-contractive visits set (to detect non-contractive
--  patterns), a contractive visits set (to terminate on infinitely receeding
--  types), and a filtering fibration (to control path exploration).  It
--  performs non-deterministic disjunctive computation to discover possible
--  binding sets and the fibrations which generate them; all such fibrations
--  will be subsumed by the filtering fibration.
checkCompatible :: forall db m.
                   ( Applicative m, ConstraintDatabase db, MonadCReader db m
                   , Display db)
                => FlowTVar
                -> T.PatternBody
                -> Visits
                -> ContractiveOccurrenceCheck
                -> Fibration db
                -> CompatM db m (PossibleBindings db, Fibration db)
checkCompatible a1 tpat visits occurrences ffib = case tpat of
  T.PPrim p -> do
    result <- liftCompat $ project (projPrim p) a1 ffib
    return $ first (\x -> if not x then Nothing else Just Map.empty) result
  T.PLabel n tpat' -> do
    -- Using full projection in case there is a nullary result
    -- TODO: implement a sort of "doesn't project" for efficiency
    (contents, fibfn, ffib's) <- liftCompat $ project (projLabel n) a1 ffib
    case (contents, ffib's) of
      ([],[]) -> return (Nothing, fibfn ffib's)
      (innerFib:_,ffib':ffib's') -> do
        (pbs, fib) <- checkCompatible innerFib tpat' visits occurrences ffib'
        return (pbs, fibfn (fib:ffib's'))
      (_,_) -> error $ "projection produced misaligned lists: "
                        ++ display contents ++ " and " ++ display ffib's
  T.PFun -> do
    result <- liftCompat $ project projFun a1 ffib
    return $ first (\x -> if null x then Nothing else Just Map.empty) result
  T.PPat -> do
    result <- liftCompat $ project projPat a1 ffib
    return $ first (\x -> if null x then Nothing else Just Map.empty) result
  T.PScape -> do
    (contents, fibfn, ffibp's) <- liftCompat $ project projScape a1 ffib
    let pbs = if null contents then Nothing else Just Map.empty
    return (pbs, fibfn ffibp's)
  T.PConj tpat1 tpat2 -> do
    -- Explore the right side first and then use the resulting fibration to
    -- filter the left side
    (pbs2, fib') <- checkCompatible a1 tpat2 visits occurrences ffib
    (pbs1, fib'') <- checkCompatible a1 tpat1 visits occurrences fib'
    case (pbs1, pbs2) of
      (Just bs1,Just bs2) -> return (Just $ Map.union bs2 bs1, fib'')
      (_, _) -> return (Nothing, fib'')
  T.PSubst a2 tpatsArg
    -- If we've already seen this pair before, we're going in circles.
    -- Step out.
    | (a1,tpat) `Set.member` occurrences -> return (Nothing, ffib)
    | otherwise -> do
      -- Using full projection in case there is a nullary result
      -- TODO: implement a sort of "doesn't project" for efficiency
      (tpatsAppl, _) <- liftCompat $ project projPat a2 Unexpanded
      case tpatsAppl of
        [] -> return (Nothing, ffib)
        (pvars, tpat'):_
          | length pvars /= length tpatsArg ->
              return (Nothing, ffib)
          | tpat' `Set.member` visits ->
              lift $ left $ NonContractivePattern tpat'
          | otherwise ->
              let tpat'' = patternSubstitute
                              (Map.fromList $ zip pvars tpatsArg) tpat' in
              let visits' = tpat' `Set.insert` visits in
              let occurrences' = (a1,tpat) `Set.insert` occurrences in
              checkCompatible a1 tpat'' visits' occurrences' ffib
  T.PRec b tpat'
    | tpat `Set.member` visits -> lift $ left $ NonContractivePattern tpat
    | otherwise ->
        let tpat'' = patternSubstitute (Map.singleton b tpat) tpat' in
        let visits' = tpat `Set.insert` visits in
        let occurrences' = (a1,tpat) `Set.insert` occurrences in
        checkCompatible a1 tpat'' visits' occurrences' ffib
  T.PVar b ->
    case ffib of
      Unexpanded -> do
        let constraints = getTypeConstraintsByUpperBound a1 <$> askDb
        TypeConstraint typ _ <- flow constraints
        return (Just $ Map.singleton b typ, blankFibrationFor typ)
      Fibration typ _ ->
        return (Just $ Map.singleton b typ, ffib)

liftCompat :: (Functor m, Monad m) => ProjM db m a -> CompatM db m a
liftCompat = flow . bimapEitherT CompatibilityProjectionError id . runFlowT
