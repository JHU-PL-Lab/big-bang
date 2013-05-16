{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
{-|
  This module implements the TinyBang constraint closure relation.
-}
module Language.TinyBang.TypeSystem.Closure
( ClosureError(..)
, calculateClosure
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Data.Maybe

import Language.TinyBang.Ast
import Language.TinyBang.Display
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.Flow
import Language.TinyBang.TypeSystem.Relations.Projection
import Language.TinyBang.TypeSystem.Types

-- |A data structure representing errors in constraint closure.
data ClosureError db
  = ClosureFailedProjection (ProjectionError db)

-- |Calculates the transitive closure of the provided constraint database.  The
--  transitive closure of a TinyBang database is only confluent up to
--  equivalence of contour folding; this function will produce a representative
--  of the appropriate equivalence class.
calculateClosure :: (ConstraintDatabase db) => db -> Either (ClosureError db) db
calculateClosure db =
  case calculateClosureStep db of
    Left err -> Left err
    Right db' ->
      if db == db' then Right db else calculateClosure db'

-- |The monad under which each closure step occurs.
type ClosureM db m = FlowT (EitherT (ClosureError db) m)

-- |A routine for expanding a @ClosureM@ monadic value.
expandClosureM :: (ConstraintDatabase db)
               => ClosureM db (CReader db) a -> db
               -> Either (ClosureError db) [a]
expandClosureM calc db =
  runCReader (runEitherT $ runFlowT calc) db

-- |Calculates a single step of closure.
calculateClosureStep :: forall db. (ConstraintDatabase db)
                     => db -> Either (ClosureError db) db
calculateClosureStep db =
  do -- Either (ClosureError db)
    dbss <- expandClosureM (sequence monotonicClosures) db
    let dbPlusMonotone = foldr (flip union) db $ concat dbss
    undefined
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
                            , Functor m, Applicative m )
                         => ClosureM db m db
closeIntegerCalculations = do
  oc@(OperationConstraint a2 _ a3 a1) <-
      flow $ lift $ getIntegerCalculationConstraints <$> askDb
  r1 <- projectSingleResult (projPrim primInt) a2
  r2 <- projectSingleResult (projPrim primInt) a3
  let history = DerivedFromClosure $ IntegerOperationRule oc r1 r2
  return $ singleton
    (WrapTypeConstraint $ TypeConstraint (Primitive primInt) a1) history

-- |Calculates integer operation comparisons in the constraint database.  This
--  does not include equality, which is more general.
closeIntegerComparisons :: ( ConstraintDatabase db, MonadCReader db m
                           , Functor m, Applicative m )
                        => ClosureM db m db
closeIntegerComparisons = do
  oc@(OperationConstraint a2 _ a3 a1) <-
      flow $ lift $ getIntegerOperationConstraints <$> askDb
  r1 <- projectSingleResult (projPrim primInt) a2
  r2 <- projectSingleResult (projPrim primInt) a3
  doEqualityFor a1 oc $ DerivedFromClosure $ IntegerCalculationRule oc r1 r2
          
-- |Calculates equality operations in the constraint database.
closeEquality :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                 , Applicative m )
              => ClosureM db m db
closeEquality = do
  oc@(OperationConstraint a2 _ a3 a1) <-
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

-- |Lifts operations into the closure monad.
liftClosure :: (Functor m, Monad m) => ProjM db m a -> ClosureM db m a
liftClosure = flow . bimapEitherT ClosureFailedProjection id . runFlowT

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
  
-- |Performs a single projection which is assumed to succeed.
projectSingleResult :: ( ConstraintDatabase db, MonadCReader db m
                       , Functor m, Applicative m )
                    => Projector -> FlowTVar
                    -> ClosureM db m (ProjectionResult db)
projectSingleResult proj a = do
  (r,f) <- liftClosure $ projectSingle proj a
  guard $ isJust r
  return $ SingleProjectionResult proj a r f
