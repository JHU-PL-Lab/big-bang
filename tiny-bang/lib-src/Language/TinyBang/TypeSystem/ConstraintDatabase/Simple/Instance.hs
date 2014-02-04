{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
  This module defines the @ConstraintDatabase@ instance for the
  @SimpleConstraintDatabase@ data type.  This is an orphan instance; we do this
  to avoid cyclic references which would be caused by the definitions of
  e.g. @Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.ReplaceVars@.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Instance
(
) where

import Control.Monad.State
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Data
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.FindVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.FreeVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Polyinst
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.ReplaceVars
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Types

instance Monoid SimpleConstraintDatabase where
  mempty = SimpleConstraintDatabase Set.empty
  mappend db1 db2 =
    foldr add db1 $ Set.toList $ query db2 QueryAllConstraints

instance ConstraintDatabase SimpleConstraintDatabase where
  add c db =
    let cCntrs = Set.map fromJust $ Set.filter isJust $ Set.map contourOfVar $
                    findAllVars db in
    let db' = SimpleConstraintDatabase $ Set.insert c $
                unSimpleConstraintDatabase db in
    contourReplaceVars cCntrs db'

  query db q =
    let cs = unSimpleConstraintDatabase db in
    case q of
      QueryAllConstraints ->
        cs
      QueryAllTVars ->
        findAllVars db
      QueryAllFreeTVars ->
        findFreeVars db
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
  
  polyinstantiate = polyinst Set.empty
