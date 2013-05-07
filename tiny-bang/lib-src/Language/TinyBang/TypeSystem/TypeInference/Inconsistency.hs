{-|
  This module provides a mechanism for detecting inconsistency in closed
  constraint databases.
-}
module Language.TinyBang.TypeSystem.TypeInference.Inconsistency
( Inconsistency(..)
, determineInconsistencies
) where

import Data.Set (Set)

import Language.TinyBang.TypeSystem.ConstraintDatabase

data Inconsistency
  = TODOInconsistency -- TODO

determineInconsistencies :: (ConstraintDatabase db) => db -> Set Inconsistency
determineInconsistencies = undefined -- TODO
