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
calculateClosure = undefined -- TODO

-- |The monad under which each closure step occurs.
type ClosureM db m = FlowT (EitherT (ClosureError db) m)

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
  
liftClosure :: (Functor m, Monad m) => ProjM db m a -> ClosureM db m a
liftClosure = flow . bimapEitherT ClosureFailedProjection id . runFlowT

-- |Calculates integer operation closures in the constraint database.
closeIntegerCalculations :: ( ConstraintDatabase db, MonadCReader db m
                            , Functor m, Applicative m )
                         => ClosureM db m db
closeIntegerCalculations = do
  oc@(OperationConstraint a2 _ a3 a1) <-
      flow $ lift $ getIntegerCalculationConstraints <$> askDb
  (r1,_) <- liftClosure $ projectSingle (projPrim primInt) a2
  guard $ isJust r1
  (r2,_) <- liftClosure $ projectSingle (projPrim primInt) a3
  guard $ isJust r2
  let history = DerivedFromClosure $ IntegerOperationRule
                    oc ProjectionResult ProjectionResult
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
  (r1,_) <- liftClosure $ projectSingle (projPrim primInt) a2
  guard $ isJust r1
  (r2,_) <- liftClosure $ projectSingle (projPrim primInt) a3
  guard $ isJust r2
  
  undefined -- TODO