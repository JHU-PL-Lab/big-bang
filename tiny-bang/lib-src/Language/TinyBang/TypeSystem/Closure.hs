{-|
  This module implements the TinyBang constraint closure relation.
-}
module Language.TinyBang.TypeSystem.Closure
( ClosureError(..)
, calculateClosure
) where

import Control.Applicative
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either

import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.Flow
import Language.TinyBang.TypeSystem.Types

-- |A data structure representing errors in constraint closure.
data ClosureError
  = TODOClosureError

-- |Calculates the transitive closure of the provided constraint database.  The
--  transitive closure of a TinyBang database is only confluent up to
--  equivalence of contour folding; this function will produce a representative
--  of the appropriate equivalence class.
calculateClosure :: (ConstraintDatabase db) => db -> Either ClosureError db
calculateClosure = undefined -- TODO

-- |Calculates transitivity closures in the constraint database.  The resulting
--  database contains the new constraints concluded from this operation.
transitivity :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                , Applicative m)
             => FlowT (EitherT ClosureError m) db
transitivity = do
  tc@(TypeConstraint t a) <- flow $ lift $ getTypeConstraints <$> askDb
  ic@(IntermediateConstraint _ a') <-
      flow $ lift $ getIntermediateConstraintsByLowerBound a <$> askDb
  let history = DerivedFromClosure $ TransitivityRule tc ic
  return $ singleton (cwrap $ TypeConstraint t a') history
