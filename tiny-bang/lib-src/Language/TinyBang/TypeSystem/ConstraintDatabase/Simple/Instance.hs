{-# LANGUAGE GADTs, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
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

import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface as CDb
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Data
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.FindVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.Polyinst
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.ReplaceVars
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.FreeVars
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours as Cntr
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

instance Monoid SimpleConstraintDatabase where
  mempty = SimpleConstraintDatabase Set.empty
  mappend db1 db2 =
    foldr add db1 $ Set.toList $ query db2 QueryAllConstraints

instance ConstraintDatabase SimpleConstraintDatabase where
  add c db =
    -- Start by ascertaining all of the contours which appear in the constraint
    let cCntrs = Set.map fromJust $ Set.filter isJust $ Set.map contourOfVar $
                    findAllVars db in
    -- Create the new database including this constraint
    let db' = SimpleConstraintDatabase $ Set.insert c $
                unSimpleConstraintDatabase db in
    -- Calculate every (unique) contour in the database
    let dbContours =
          Set.fromList $ mapMaybe contourOfVar $ Set.toList $ findAllVars db in
    -- Next, determine if any of the input contours overlap with each other
    let cntrs' = Set.map (`mergeIfOverlap` Set.toList cCntrs) cCntrs in
    -- Next, calculate the overlap for each of the existing contours with these
    -- new ones
    let cntrs'' = Set.map (`mergeIfOverlap` Set.toList dbContours) cntrs' in
    -- This new set of contours is disjoint and each contour in it entirely
    -- subsumes any contour in the constraint database that it overlaps.
    -- Therefore, any contour in the database which overlaps a contour in this new
    -- set should be replaced by the latter contour.  Do that now.
    -- TODO: remove the following debug logs (or clean them up)
    _debugI
      (display $ align $
        text "cCntrs:" <+> makeDoc cCntrs </>
        text "dbContours:" <+> makeDoc dbContours </>
        text "cntrs':" <+> makeDoc cntrs' </>
        text "cntrs'':" <+> makeDoc cntrs'')
      $ replaceVarsByContours cntrs'' db'
    where
      mergeIfOverlap :: Contour -> [Contour] -> Contour
      mergeIfOverlap cntr others =
        foldl Cntr.union cntr $
          filter (\cntr' -> overlap cntr cntr' && cntr /= cntr') others
    
  addWithExistingContours = add

  unionWithExistingContours = CDb.union

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
  
  polyinstantiate = polyinst


$(createFindFreeVarsInstances ''SimpleConstraintDatabase)
$(createReplaceVarsInstances ''SimpleConstraintDatabase)
$(createPolyinstInstances ''SimpleConstraintDatabase)
