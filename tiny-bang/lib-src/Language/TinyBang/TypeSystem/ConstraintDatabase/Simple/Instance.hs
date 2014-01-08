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
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Data
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.FindVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.FreeVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Polyinst
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.ReplaceVars
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Types

instance ConstraintDatabase SimpleConstraintDatabase where
  empty = SimpleConstraintDatabase Set.empty
  
  add c db =
    let newVars = findAllVars c in
    let newCntrs = Set.fromList $ mapMaybe contourOfVar $ Set.toList newVars in
    contourReplaceVars newCntrs $
      SimpleConstraintDatabase $ Set.insert c $ unSimpleConstraintDatabase db

  union db1 db2 =
    foldr add db1 $ Set.toList $ query db2 QueryAllConstraints

  query db q =
    let cs = unSimpleConstraintDatabase db in
    case q of
      QueryAllConstraints ->
        cs
      QueryAllFreeTVars ->
        findFreeVars db
      QueryAllTypesLowerBoundingTVars -> Set.fromList $ do
        TypeConstraint _ t a' <- Set.toList cs
        return (t, a')
      QueryAllApplications -> Set.fromList $ do
        ApplicationConstraint _ a0 a1 a2 <- Set.toList cs
        return (a0, a1, a2)
      QueryAllInconsistencies -> Set.fromList $ do
        InconsistencyConstraint _ i <- Set.toList cs
        return i
      QueryLowerBoundingTypesOfTVar a -> Set.fromList $ do
        TypeConstraint _ t a' <- Set.toList cs
        guard $ a == a'
        return t
      QueryLowerBoundingTVarsOfTVar a -> Set.fromList $ do
        IntermediateConstraint _ a1 a2 <- Set.toList cs
        guard $ a == a2
        return a1
  
  polyinstantiate = polyinst Set.empty
