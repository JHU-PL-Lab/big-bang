module Language.TinyBang.TypeSystem.Data.Compatibility
where -- TODO
{-
( CompatibilityArgument(..)
) where

import Language.TinyBang.Display
import Language.TinyBang.TypeSystem.Types

-- |A data structure representing the argument type passed to a pattern
--  compatibility check.
data CompatibilityArgument
  = ArgVal FlowTVar
  | ArgExn FlowTVar
  deriving (Eq, Ord, Show)
  
instance Display CompatibilityArgument where
  makeDoc arg = case arg of
    ArgVal a -> makeDoc a
    ArgExn a -> text "exn" <+> makeDoc a
-}