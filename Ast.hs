module Ast
( Expr(..)
, Chi(..)
, Branches
, Branch
) where

import qualified Types as T
import UtilTypes (LabelName, Ident)

-------------------------------------------------------------------------------

-- |Data type for representing Big Bang ASTs.
data Expr =
      Var Ident
    | Label LabelName Expr
    | Onion Expr Expr
    | Func Ident Expr
    | Appl Expr Expr
    | PrimInt Integer
    | PrimChar Char
    | PrimUnit
    | Case Expr Branches
    -- Below are the AST forms which cannot be represented as text directly
    | Plus Expr Expr
    | Minus Expr Expr
    | Equal Expr Expr
    deriving (Show)

-- |Data type describing type patterns for case expressions.
data Chi =
      ChiPrim (T.PrimitiveType)
    | ChiLabel LabelName Ident
    | ChiOnion Ident Ident
    | ChiFun
    deriving (Show)

-- |Alias for case branches
type Branches = [Branch]
type Branch = (Chi,Expr)
