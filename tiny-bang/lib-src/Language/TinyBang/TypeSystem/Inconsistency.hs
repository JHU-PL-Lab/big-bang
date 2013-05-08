{-|
  This module provides a mechanism for detecting inconsistency in closed
  constraint databases.
-}
module Language.TinyBang.TypeSystem.Inconsistency
( Inconsistency(..)
, determineInconsistencies
) where

import Data.Set (Set)

import Language.TinyBang.TypeSystem.Closure.Database

data Inconsistency
  = TODOInconsistency -- TODO

determineInconsistencies :: (ConstraintDatabase db) => db -> Set Inconsistency
determineInconsistencies = undefined -- TODO
