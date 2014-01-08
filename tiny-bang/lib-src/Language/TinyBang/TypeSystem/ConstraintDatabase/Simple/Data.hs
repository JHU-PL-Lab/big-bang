{-|
  Defines the basic data type for the simple constraint database.  Users
  should import @Language.TinyBang.TypeSystem.ConstraintDatabase.Simple@
  instead of this module; this module does not include the typeclass instances
  for the type.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Data
( SimpleConstraintDatabase(..)
, unSimpleConstraintDatabase
) where

import Data.Set (Set)

import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.Utils.Display

newtype SimpleConstraintDatabase
  = SimpleConstraintDatabase (Set (Constraint SimpleConstraintDatabase))
  deriving (Eq, Ord, Show)

unSimpleConstraintDatabase :: SimpleConstraintDatabase -> Set (Constraint SimpleConstraintDatabase)
unSimpleConstraintDatabase (SimpleConstraintDatabase s) = s

instance Display SimpleConstraintDatabase where
  makeDoc = makeDoc . unSimpleConstraintDatabase
