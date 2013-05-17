module Language.TinyBang.TypeSystem.Data.Compatibility
( CompatibilityArgument(..)
) where

import Language.TinyBang.TypeSystem.Types

-- |A data structure representing the argument type passed to a pattern
--  compatibility check.
data CompatibilityArgument
  = ArgVal FlowTVar
  | ArgExn FlowTVar
  deriving (Eq, Ord, Show)