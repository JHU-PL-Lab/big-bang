{-|
  Contains basic definitions for projection, a relation used to support the
  definition of built-in operations.
-}
module Language.TinyBang.Utils.Projection
( Projector(..)
, builtinVar
) where

-- TODO: deprecate this module.  The evaluation process should be using some
--       form of strict pattern matching rather than reinventing this process
--       using projection.

import Language.TinyBang.Ast

data Projector
  = ProjPrim PrimitiveType
  | ProjLabel LabelName
  | ProjRef
  deriving (Eq, Ord, Show)

builtinVar :: BuiltinOp -> Var
builtinVar op = Var generated (BuiltinOutputVar op) Nothing

