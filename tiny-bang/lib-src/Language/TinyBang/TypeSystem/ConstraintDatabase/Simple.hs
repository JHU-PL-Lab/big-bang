{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, GADTs #-}

{-|
  This module carries an implementation of the constraint database which uses
  a simple backing set of constraints.  Lookups and similar operations are
  O(n) time; no smart data structures are used.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple
( SimpleConstraintDatabase
) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.FreeVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Polyinst
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.Utils.Display

newtype SimpleConstraintDatabase
  = SimpleConstraintDatabase (Set (Constraint SimpleConstraintDatabase))
  deriving (Eq, Ord, Show)

unSimpleConstraintDatabase :: SimpleConstraintDatabase -> Set (Constraint SimpleConstraintDatabase)
unSimpleConstraintDatabase (SimpleConstraintDatabase s) = s

-- TODO: contours for every add or union

instance ConstraintDatabase SimpleConstraintDatabase where
  empty = SimpleConstraintDatabase Set.empty
  
  add c = SimpleConstraintDatabase . Set.insert c . unSimpleConstraintDatabase

  union db1 db2 =
    SimpleConstraintDatabase $
      Set.union (unSimpleConstraintDatabase db1)
                (unSimpleConstraintDatabase db2)

  query db q =
    let cs = unSimpleConstraintDatabase db in
    case q of
      QueryAllConstraints ->
        cs
      QueryAllFreeTVars ->
        findFreeVars unSimpleConstraintDatabase db
      QueryAllTypesLowerBoundingTVars -> Set.fromList $ do
        TypeConstraint _ t a' <- Set.toList cs
        return (t, a')
      QueryAllApplications -> Set.fromList $ do
        ApplicationConstraint _ a0 a1 a2 <- Set.toList cs
        return (a0, a1, a2)
      QueryLowerBoundingTypesOfTVar a -> Set.fromList $ do
        TypeConstraint _ t a' <- Set.toList cs
        guard $ a == a'
        return t
      QueryLowerBoundingTVarsOfTVar a -> Set.fromList $ do
        IntermediateConstraint _ a1 a2 <- Set.toList cs
        guard $ a == a2
        return a1
  
  polyinstantiate =
    polyinst unSimpleConstraintDatabase SimpleConstraintDatabase Set.empty


instance Display SimpleConstraintDatabase where
  makeDoc = makeDoc . unSimpleConstraintDatabase
