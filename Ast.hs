module Ast
(
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
    | PrimString String
    | PrimUnit
    | Case Expr [(Chi,Expr)]

-- |Data type describing type patterns for case expressions.
data Chi =
      ChiPrim (T.PrimitiveType)
    | ChiLabel Ident
    | ChiOnion Ident Ident
    | ChiFun

