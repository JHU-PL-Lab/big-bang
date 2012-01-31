{-# LANGUAGE FlexibleInstances#-}
module Language.MicroBang.Ast
( Expr(..)
, Operator(..)
, Chi(..)
, Branches
, Branch(..)
, Value(..)
, exprFromValue
, Evaluated(..)
) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Language.MicroBang.Types.UtilTypes as T
import Utils.Render.Display
  
-------------------------------------------------------------------------------

-- |Data type for representing Big Bang ASTs.
data Expr
  = Var T.Ident
  | Label T.LabelName Expr
  | Onion Expr Expr
  | OnionSub Expr T.SubTerm
  | EmptyOnion
  | Func T.Ident Expr
  | Appl Expr Expr
  | PrimInt Integer
  | PrimUnit
  | Case Expr Branches
  | BinOp Operator Expr Expr
  deriving (Eq, Ord, Show)

-- |Data type for representing Big Bang operators.
data Operator
  = Equal
  | Plus
  deriving (Eq, Ord, Show)

-- |Data type for representing Big Bang values
data Value
  = VLabel T.LabelName Value
  | VOnion Value Value
  | VFunc T.Ident Expr
  | VPrimInt Integer
  | VPrimUnit
  | VEmptyOnion
  deriving (Eq, Ord, Show)

-- |Data type describing type patterns for case expressions.
data Chi
  = ChiPrim T.PrimitiveType
  | ChiLabel T.LabelName T.Ident
  | ChiFun
  | ChiAny
  deriving (Eq, Ord, Show)

-- |Alias for case branches
type Branches = [Branch]
data Branch = Branch (Maybe T.Ident) Chi Expr
  deriving (Eq, Ord, Show)

-- |Trivial conversion from values to exprs
exprFromValue :: (Evaluated v) => v -> Expr
exprFromValue v = case value v of
  VLabel l v1  -> Label l $ exprFromValue v1
  VOnion v1 v2 -> Onion (exprFromValue v1) (exprFromValue v2)
  VFunc i e    -> Func i e
  VPrimInt i   -> PrimInt i
  VPrimUnit    -> PrimUnit
  VEmptyOnion  -> OnionSub PrimUnit $ T.SubPrim T.PrimUnit

instance Display Expr where
  makeDoc a = case a of
    Var i -> text $ T.unIdent i
    Label n e -> char '`' <> (text $ T.unLabelName n) <+> makeDoc e
    Onion e1 e2 -> makeDoc e1 <+> char '&' <+> makeDoc e2
    Func i e -> parens $
            text "fun" <+> (text $ T.unIdent i) <+> text "->" <+> makeDoc e
    Appl e1 e2 -> parens $ makeDoc e1 <+> makeDoc e2
    PrimInt i -> integer i
    PrimUnit -> parens empty
    Case e brs -> text "case" <+> makeDoc e <+> text "of" <+> text "{" $+$
            (nest indentSize $ vcat $ punctuate semi $ map makeDoc brs)
            $+$ text "}"
    OnionSub e s -> makeDoc e <+> char '&' <> makeDoc s
    EmptyOnion -> text "(&)"
    BinOp op e1 e2 -> makeDoc op <+> makeDoc e1 <+> makeDoc e2

instance Display Operator where
  makeDoc x =
    case x of
      Equal -> text "[=]"
      Plus -> text "[+]"

instance Display Value where
  makeDoc x =
    case x of
      VEmptyOnion -> text "(&)"
      _ -> makeDoc $ exprFromValue x

instance Display Branch where
  makeDoc (Branch mident chi e) =
    maybe empty ((<> colon) . text . T.unIdent) mident <+>
    (case chi of
      ChiPrim p -> makeDoc p
      ChiLabel n i ->
        char '`' <> (text $ T.unLabelName n) <+> (text $ T.unIdent i)
      ChiFun -> text "fun"
      ChiAny -> text "_"
    ) <+> text "->" <+> makeDoc e

class Evaluated a where
  value :: a -> Value
  value v = fst $ vmPair v
  mapping :: a -> IntMap Value
  mapping v = snd $ vmPair v

  vmPair :: a -> (Value, IntMap Value)
  vmPair v = (value v, mapping v)

instance Evaluated (Value, IntMap Value) where
  vmPair = id

instance Evaluated Value where
  value = id
  mapping = const IntMap.empty
