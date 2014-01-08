{-|
  This module carries an implementation of the constraint database which uses
  a simple backing set of constraints.  Lookups and similar operations are
  O(n) time; no smart data structures are used.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple
( SimpleConstraintDatabase
) where

import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Data
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Instance()
