{-|
  Contains basic definitions for projection, a relation used to support the
  definition of built-in operations.
-}
module Language.TinyBang.Utils.Projection
( Projector(..)
, builtinVar
) where

import Language.TinyBang.Ast

data Projector
  = ProjPrim PrimitiveType
  | ProjLabel LabelName
  | ProjRef
  deriving (Eq, Ord, Show)

builtinVar :: BuiltinOp -> Var
builtinVar op = Var generated (BuiltinVar op) Nothing

