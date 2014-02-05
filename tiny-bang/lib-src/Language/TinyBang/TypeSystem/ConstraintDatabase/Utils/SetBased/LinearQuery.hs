{-# LANGUAGE GADTs, ConstraintKinds, FlexibleContexts #-}

{-|
  Contains a naive linear query implementation for constraint sets.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.LinearQuery
( linearQuery
) where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.FindVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.FreeVars
import Language.TinyBang.TypeSystem.Constraints

-- |Defines a linear-time query on a set of constraints.
linearQuery :: (Ord db, FindAllVarsable db, FindFreeVarsable db)
            => Set (Constraint db) -> ConstraintQuery db r -> Set r
linearQuery cs q =
    case q of
      QueryAllConstraints ->
        cs
      QueryAllTVars ->
        findAllVars cs
      QueryAllFreeTVars ->
        findFreeVars cs
      QueryAllTypesLowerBoundingTVars -> Set.fromList $ do
        TypeConstraint _ t a' <- Set.toList cs
        return (t, a')
      QueryAllApplications -> Set.fromList $ do
        ApplicationConstraint _ a0 a1 a2 <- Set.toList cs
        return (a0, a1, a2)
      QueryAllBuiltins -> Set.fromList $ do
        BuiltinConstraint _ op as a <- Set.toList cs
        return (op, as, a)
      QueryAllInconsistencies -> Set.fromList $ do
        InconsistencyConstraint _ i <- Set.toList cs
        return i
      QueryLowerBoundingTypesOfTVar a -> Set.fromList $ do
        TypeConstraint _ t a' <- Set.toList cs
        guard $ a == a'
        return t
      QueryUpperBoundingTVarsOfTVar a -> Set.fromList $ do
        IntermediateConstraint _ a1 a2 <- Set.toList cs
        guard $ a == a1
        return a2
