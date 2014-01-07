{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances, GeneralizedNewtypeDeriving  #-}
{-|
  This module implements the TinyBang constraint closure relation.
-}
module Language.TinyBang.TypeSystem.Closure
( calculateClosure
) where

import Control.Applicative
import Control.Monad
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Matching
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.NonDet
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

{- TODO: improve this implementation.  At the moment, a lot of cases are being
         examined multiple times.  One possibility is a two-level implementation
         in which the inner level operates under a "sloppy" cache, ignoring
         e.g. call sites which have already been visited.  The outer level can
         then clear the cache at each step and proceed until no progress is
         made even with an empty cache. 
-}

-- |Calculates the transitive closure of the provided constraint set under the
--  TinyBang constraint closure rules.
calculateClosure :: (ConstraintDatabase db, Display db)
                 => db -> db
calculateClosure db =
  let db' = calculateClosureStep db in
  if db == db' then db else calculateClosure db

-- |Calculates a single step of constraint closure.
calculateClosureStep :: forall db. (ConstraintDatabase db, Display db)
                     => db -> db
calculateClosureStep db =
  foldr CDb.union db $ concat $ runClosureStepM (sequence rules) db
  where
    rules :: [ClosureStepM db db]
    rules =
      [ closeTransitivity
      , closeApplication
      ]

newtype ClosureStepM db a
  = ClosureStepM
      { unClosureStepM :: NonDetT (CReader db) a
      }
  deriving ( Monad, Functor, Applicative, MonadCReader db, MonadNonDet
           , MonadPlus)

runClosureStepM :: (ConstraintDatabase db, Display db) => ClosureStepM db a -> db -> [a]
runClosureStepM x db = runCReader (runNonDetT (unClosureStepM x)) db

closeTransitivity :: (ConstraintDatabase db, Display db) => ClosureStepM db db
closeTransitivity = do
  (t,a1) <- join $ choose <$> queryDb QueryAllTypesLowerBoundingTVars
  a2 <- join $ choose <$> queryDb (QueryLowerBoundingTVarsOfTVar a1)
  let h = DerivedFromClosure $ TransitivityRule t a1 a2
  return $ CDb.singleton $ t <: a2 .: h

closeApplication :: forall db. (ConstraintDatabase db, Display db)
                 => ClosureStepM db db
closeApplication = do
  (a0,a1,a2) <- join $ choose <$> queryDb QueryAllApplications
  db <- askDb
  (argtov, mbinds) <- choose $ matches a2 a0 a1 db
  case mbinds of
    Nothing ->
      -- TODO: perhaps implement contradictions as constraints?
      mzero
    Just (scapet, (a',db')) ->
      let h = DerivedFromClosure $ ApplicationRule a0 a1 a2 scapet argtov in
      let db'' = CDb.add (a' <: a2 .: h) db' in
      let cntr = undefined in -- TODO
      return $ CDb.polyinstantiate cntr db''

{-

-- |Calculates a single step of closure.
calculateClosureStep :: forall db. (ConstraintDatabase db, Display db, Ord db)
                     => db -> Either (ClosureError db) db
calculateClosureStep db =
  do -- Either (ClosureError db)
    dbss <- mapM (`expandClosureM` db) monotonicClosures
    _debug $ "Monotonic closure rules gave: " ++ display (debugFiltering dbss)
    let dbPlusMonotone = foldr (flip union) db $ concat dbss 
    answer <- if dbPlusMonotone /= db
                then return dbPlusMonotone
                else calculateApplicationClosure db
    _debug $ "Closure step yields: " ++ display answer
    _debug $ "Contours in current database: " ++ display (getAllContours answer)
    return answer
  where
    monotonicClosures :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                         , Applicative m)
                      => [ClosureM db m db]
    monotonicClosures = [ closeTransitivity
                        , closeIntegerCalculations
                        , closeIntegerComparisons
                        , closeEquality
                        , closeCellPropagation
                        , closeExceptionPropagation
                        ]
    debugFiltering :: [[db]] -> [Constraint db]
    debugFiltering dbss = concat $ concat $ (`map` dbss) $ map $
      Set.toList . (`Set.difference` getAllConstraints db) . getAllConstraints

-- * Application closure

-- |Calculates application closure on the given database.  This routine will
--  perform some number of application closures but may not perform all of them.
--  Nonetheless, this function guarantees that progress will be made if any
--  progress is possible; if the input and output are equal, the exist no
--  additional application closures to perform.
calculateApplicationClosure :: forall db. (ConstraintDatabase db, Display db)
                            => db -> Either (ClosureError db) db
calculateApplicationClosure db =
  do -- Either (ClosureError db)
    effects <- mapM (`expandClosureM` db) applicationClosures
    return $ applyEffects $ concat effects
  where
    applicationClosures :: ( Display db, ConstraintDatabase db
                           , MonadCReader db m , Functor m, Applicative m)
                        => [ClosureM db m (db, Maybe Contour)]
    applicationClosures = [ normalApplicationClosures
                          , exceptionalApplicationClosures ]
    applyEffects effects = case effects of
      [] -> db
      (db', mcn):effects' ->
        let f = case mcn of
                  Nothing -> id
                  Just cn -> replaceContours cn
        in
        let db'' = f $ db `union` db' in
        if db'' /= db
          then db''
          else applyEffects effects'

-- |Determines the effects of normal application closure in a given database.
--  This operation produces pairs of constraint sets and contours to apply to
--  the database, but it does not perform any replacement.  If the contour is a
--  @Nothing@, no replacement is to occur.  The resulting list of closures is
--  produced in arbitrary order.
normalApplicationClosures :: forall db m.
                             ( Display db, ConstraintDatabase db
                             , MonadCReader db m , Functor m, Applicative m)
                          => ClosureM db m (db, Maybe Contour)
normalApplicationClosures = do
  appc@(ApplicationConstraint a1 a2 a3) <-
      flow $ lift $ getApplicationConstraints <$> askDb
  (scapes, pFib) <- liftProjToClosure $ project projFun a1
  let arg = ArgVal a2
  (mdata@(Just (a4, db')), cFib) <- liftCompatToClosure $
                  checkApplicationCompatible arg scapes
  cn <- cNew a3 <$> askDb
  let wiringConstraint = cwrap $ IntermediateConstraint a4 a3
      wiringHistory = DerivedFromClosure $ ApplicationRule appc
                        (ProjectionResult projFun a1 $
                          ProjectionResultFunForm scapes pFib)
                        (ApplicationCompatibilityResult arg scapes mdata cFib)
                        cn
  let db'' = instantiateContours Set.empty cn $
                add wiringConstraint wiringHistory db'
  return (db'', Just cn)

-- |Determines the effects of exceptional application closure in a given
--  database.  This function parallels @normalApplicationClosures@ but
--  handles cases in which exceptions are caught (or not caught).
exceptionalApplicationClosures :: forall db m.
                                  ( Display db, ConstraintDatabase db
                                  , MonadCReader db m , Functor m
                                  , Applicative m)
                               => ClosureM db m (db, Maybe Contour)
exceptionalApplicationClosures = do
  appc@(ApplicationConstraint a1 a2 a3) <-
      flow $ lift $ getApplicationConstraints <$> askDb
  (scapes, pFib) <- liftProjToClosure $ project projFun a1
  ec@(ExceptionConstraint a4 _) <-
      flow $ lift $ getExceptionConstraintsByUpperBound a2 <$> askDb
  let arg = ArgExn a4
  (mdata, cFib) <- liftCompatToClosure $ checkApplicationCompatible arg scapes
  let mprojRes = ProjectionResult projFun a1 $
                    ProjectionResultFunForm scapes pFib
  let appCRes = ApplicationCompatibilityResult arg scapes mdata cFib
  case mdata of
    Nothing ->
      let db' = singleton (cwrap $ ExceptionConstraint a4 a3)
            (DerivedFromClosure $ ExceptionPassRule appc ec mprojRes appCRes) in
      return (db', Nothing)
    Just (a5, db') -> do
      cn <- cNew a3 <$> askDb
      let wiringConstraint = cwrap $ IntermediateConstraint a5 a3
          wiringHistory = DerivedFromClosure $
                              ExceptionCatchRule appc ec mprojRes appCRes cn
      let db'' = instantiateContours Set.empty cn $
                    add wiringConstraint wiringHistory db'
      return (db'', Just cn)

-- * Closure routines

-- |Calculates transitivity closures in the constraint database.  The resulting
--  databases contain the new constraints concluded from this operation.
closeTransitivity :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                     , Applicative m)
                  => ClosureM db m db
closeTransitivity = do
  tc@(TypeConstraint t a) <- flow $ lift $ getTypeConstraints <$> askDb
  ic@(IntermediateConstraint _ a') <-
      flow $ lift $ getIntermediateConstraintsByLowerBound a <$> askDb
  let history = DerivedFromClosure $ TransitivityRule tc ic
  return $ singleton (cwrap $ TypeConstraint t a') history

-- |Calculates integer operation closures in the constraint database.
closeIntegerCalculations :: ( ConstraintDatabase db, MonadCReader db m
                            , Functor m, Applicative m, Display db )
                         => ClosureM db m db
closeIntegerCalculations = do
  oc@(OperationConstraint a2 _ a3 a1) <-
      flow $ lift $ getIntegerCalculationConstraints <$> askDb
  (True,r1) <- liftProjToClosure $ projectSinglePrimResult projInt a2
  (True,r2) <- liftProjToClosure $ projectSinglePrimResult projInt a3
  let history = DerivedFromClosure $ IntegerOperationRule oc r1 r2
  return $ singleton
    (WrapTypeConstraint $ TypeConstraint (Primitive primInt) a1) history

-- |Calculates integer operation comparisons in the constraint database.  This
--  does not include equality, which is more general.
closeIntegerComparisons :: ( ConstraintDatabase db, MonadCReader db m
                           , Functor m, Applicative m, Display db )
                        => ClosureM db m db
closeIntegerComparisons = do
  oc@(OperationConstraint a2 _ a3 a1) <-
      flow $ lift $ getIntegerOperationConstraints <$> askDb
  (True,r1) <- liftProjToClosure $ projectSinglePrimResult projInt a2
  (True,r2) <- liftProjToClosure $ projectSinglePrimResult projInt a3
  doEqualityFor a1 oc $ DerivedFromClosure $ IntegerCalculationRule oc r1 r2

-- |Calculates equality operations in the constraint database.
closeEquality :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                 , Applicative m )
              => ClosureM db m db
closeEquality = do
  oc@(OperationConstraint _ _ _ a1) <-
      flow $ lift $ getEqualityConstraints <$> askDb
  doEqualityFor a1 oc $ DerivedFromClosure $ EqualityRule oc

-- |Calculates cell propagation closure.
closeCellPropagation :: ( ConstraintDatabase db, MonadCReader db m
                        , Functor m, Applicative m )
                     => ClosureM db m db
closeCellPropagation = do
  lc@(CellLoadingConstraint b a) <-
      flow $ lift $ getCellLoadingConstraints <$> askDb
  lbc <- flow $ lift $ getCellLowerBoundConstraints b <$> askDb
  let a' = lowerBoundOf lbc
  let history = DerivedFromClosure $ CellPropagationRule lc lbc
  return $ singleton (cwrap $ IntermediateConstraint a' a) history

-- |Calculates exception propagation closure.
closeExceptionPropagation :: ( ConstraintDatabase db, MonadCReader db m
                             , Functor m, Applicative m )
                          => ClosureM db m db
closeExceptionPropagation = do
  ec@(ExceptionConstraint a2 a1) <-
      flow $ lift $ getExceptionConstraints <$> askDb
  fc@(FlowConstraint _ _ a3) <-
      flow $ lift $ getFlowConstraintsByLowerBound a1 FlowExn <$> askDb
  let history = DerivedFromClosure $ ExceptionPropagationRule ec fc
  return $ singleton (cwrap $ ExceptionConstraint a2 a3) history

-- * Utility functions

-- |Lifts a projection operation into the closure monad.
liftProjToClosure :: (Functor m, Monad m) => ProjM db m a -> ClosureM db m a
liftProjToClosure = flow . bimapEitherT ClosureFailedProjection id . runFlowT

-- |Lifts a compatibility operation into the closure monad.
liftCompatToClosure :: (Functor m, Monad m) => CompatM db m a -> ClosureM db m a
liftCompatToClosure = flow . bimapEitherT ClosureFailedProjection id . runFlowT

-- |Creates a database with equality constraints over the specified type
--  variable.
doEqualityFor :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                 , Applicative m )
              => FlowTVar -> OperationConstraint -> ConstraintHistory db
              -> ClosureM db m db
doEqualityFor a1 oc history =
  case a1 of
    GenFlowTVar _ _ ->
      error $ "How did a generated flow variable (" ++ display a1
        ++ ") appear as the upper bound of an integer comparison ("
        ++ display oc ++ ")?"
    FlowTVar x pc ->
      let a' = GenFlowTVar x pc in
      let b' = GenCellTVar x pc in
      let labelTrue = LabelName (ComputedOrigin []) "True" in
      let labelFalse = LabelName (ComputedOrigin []) "False" in
      return $ fromList $ map (,history)
          [ WrapTypeConstraint $ TypeConstraint EmptyOnion a'
          , WrapCellCreationConstraint $ CellCreationConstraint a' b'
          , WrapTypeConstraint $ TypeConstraint (Label labelTrue b') a1
          , WrapTypeConstraint $ TypeConstraint (Label labelFalse b') a1
          ]
-}
