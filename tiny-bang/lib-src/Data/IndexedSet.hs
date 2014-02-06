{-|
  This module defines routines and utilities associated with the creation and
  use of a data structure which is equivalent to a set but maintains a number of
  indexes for quick containment checks.  The containment checks must be
  enumerated statically.
-}
module Data.IndexedSet
( module X
) where

import Data.IndexedSet.Class as X
import Data.IndexedSet.TemplateHaskell as X
