{-|
  This module provides the interface for constraint databases.
-}

module Language.TinyBang.TypeSystem.Closure.Database
( ConstraintDatabase(..)
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types

-- |A typeclass defining the interface for constraint databases.
class ConstraintDatabase db where
  -- |Obtains an empty constraint database.
  empty :: db
  -- |Adds a new constraint to a database.
  add :: db -> Constraint db -> ConstraintHistory db -> db
  -- |Unions two databases.  If there is a performance difference between the
  --  ordering of the arguments, it will generally be in favor of the first
  --  database being the larger one.
  union :: db -> db -> db
  -- |Obtains the set of contours which appear in a database.
  getAllContours :: db -> Set Contour
  -- |Performs contour replacement on the contents of a database.
  replaceContours :: Contour -> db -> db
  -- TODO: the rest of the interface
  -- ### Convenience functions
  -- |Creates a singleton constraint database.  By default, this simply adds
  --  a constraint to an empty database.
  singleton :: Constraint db -> ConstraintHistory db -> db
  singleton = add empty
