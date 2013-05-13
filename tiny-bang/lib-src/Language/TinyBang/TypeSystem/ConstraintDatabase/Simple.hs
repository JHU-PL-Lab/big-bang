{-|
  This module carries an implementation of the constraint database which uses
  a simple backing set of constraints.  Lookups and similar operations are
  O(n) time; no smart data structures are used.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple
( SimpleConstraintDatabase
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Relations
import Language.TinyBang.TypeSystem.Types

data SimpleConstraintDatabase
  = SimpleConstraintDatabase
      (Set (Constraint SimpleConstraintDatabase))
      (Map
        (Constraint SimpleConstraintDatabase)
        (ConstraintHistory SimpleConstraintDatabase))
  deriving (Eq, Ord, Show)

instance ConstraintDatabase SimpleConstraintDatabase where
  empty = SimpleConstraintDatabase Set.empty Map.empty
  add c hist (SimpleConstraintDatabase cs hists) =
    SimpleConstraintDatabase (Set.insert c cs) (Map.insert c hist hists)
  union (SimpleConstraintDatabase cs hists)
        (SimpleConstraintDatabase cs' hists') =
    SimpleConstraintDatabase (cs `Set.union` cs') (hists `Map.union` hists')
  getAllContours (SimpleConstraintDatabase cs _) = extractContours cs

  getLowerBounds a (SimpleConstraintDatabase cs _) =
    Set.fromList $ mapMaybe lowerBoundConstraintMapper (Set.toList cs)
    where
      lowerBoundConstraintMapper c = case c of
        WrapTypeConstraint (TypeConstraint t a') | a == a' -> Just t
        _ -> Nothing
