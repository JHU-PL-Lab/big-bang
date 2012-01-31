{-# LANGUAGE FlexibleInstances#-}
module Language.TinyBang.Ast
( Expr(..)
, Chi(..)
, Branches
, Branch(..)
, Value(..)
-- Re-exported for convenience
, LazyOperator(..)
, EagerOperator(..)
, SubTerm(..)
, exprFromValue
, Assignable(..)
, Evaluated(..)
, CellId
) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Language.TinyBang.Types.UtilTypes
  ( LabelName
  , Ident
  , unIdent
  , unLabelName
  , LazyOperator(..)
  , EagerOperator(..)
  , SubTerm(..)
  )
import qualified Language.TinyBang.Types.UtilTypes as T
  ( PrimitiveType(..) )
import Utils.Render.Display
  
-------------------------------------------------------------------------------

type CellId = Int

-- |Data type for representing Big Bang ASTs.
data Expr
  = Var Ident
  | Label LabelName Expr
  | Onion Expr Expr
  | OnionSub Expr SubTerm
  | Func Ident Expr
  | Appl Expr Expr
  | PrimInt Integer
  | PrimChar Char
  | PrimUnit
  | Case Expr Branches
  | Def Ident Expr Expr
  | Assign Assignable Expr Expr
  | LazyOp LazyOperator Expr Expr
  | EagerOp EagerOperator Expr Expr
  | ExprCell CellId
  deriving (Eq, Ord, Show)

-- |Data type for representing Big Bang values
data Value
  = VLabel LabelName CellId
  | VOnion Value Value
  | VFunc Ident Expr
  | VPrimInt Integer
  | VPrimChar Char
  | VPrimUnit
  | VEmptyOnion
  deriving (Eq, Ord, Show)

data Assignable = ACell CellId | AIdent Ident
  deriving (Eq, Ord, Show)

-- |Data type describing type patterns for case expressions.
data Chi
  = ChiPrim T.PrimitiveType
  | ChiLabel LabelName Ident
  | ChiFun
  | ChiAny
  deriving (Eq, Ord, Show)

-- |Alias for case branches
type Branches = [Branch]
data Branch = Branch (Maybe Ident) Chi Expr
  deriving (Eq, Ord, Show)

-- |Trivial conversion from values to exprs
exprFromValue :: (Evaluated v) => v -> Expr
exprFromValue v = case value v of
  VLabel l c   -> Label l $ ExprCell c
  VOnion v1 v2 -> Onion (exprFromValue v1) (exprFromValue v2)
  VFunc i e    -> Func i e
  VPrimInt i   -> PrimInt i
  VPrimChar c  -> PrimChar c
  VPrimUnit    -> PrimUnit
  VEmptyOnion  -> OnionSub PrimUnit $ SubPrim T.PrimUnit -- TODO: EEmptyOnion

instance Display Expr where
  makeDoc a = case a of
    Var i -> text $ unIdent i
    Label n e -> char '`' <> (text $ unLabelName n) <+> makeDoc e
    Onion e1 e2 -> makeDoc e1 <+> char '&' <+> makeDoc e2
    Func i e -> parens $
            text "fun" <+> (text $ unIdent i) <+> text "->" <+> makeDoc e
    Appl e1 e2 -> parens $ makeDoc e1 <+> makeDoc e2
    PrimInt i -> integer i
    PrimChar c -> quotes $ char c
    PrimUnit -> parens empty
    Case e brs -> text "case" <+> makeDoc e <+> text "of" <+> text "{" $+$
            (nest indentSize $ vcat $ punctuate semi $ map makeDoc brs)
            $+$ text "}"
    OnionSub e s -> makeDoc e <+> char '&' <> makeDoc s
    LazyOp op e1 e2 -> makeDoc op <+> makeDoc e1 <+> makeDoc e2
    EagerOp op e1 e2 -> makeDoc op <+> makeDoc e1 <+> makeDoc e2
    {-
       TODO: deal with the fact that the following isn't actually code
       options include:
           * errors on ASTs containing builtin nodes
           * namespacing trick (e.g., Plus translates to
               "Language.TinyBang.Builtins.([+]) e1 e2"
    -}
    Def i v e -> hsep [text "def", makeDoc i, text "=", makeDoc v, text "in", makeDoc e]
    Assign i v e -> hsep [makeDoc i, text "=", makeDoc v, text "in", makeDoc e]
    ExprCell c -> text "Cell #" <> int c

instance Display Value where
  makeDoc x =
    case x of
      VEmptyOnion -> text "(&)"
      _ -> makeDoc $ exprFromValue x

instance Display Branch where
  makeDoc (Branch mident chi e) =
    maybe empty ((<> colon) . text . unIdent) mident <+>
    (case chi of
      ChiPrim p -> makeDoc p
      ChiLabel n i ->
        char '`' <> (text $ unLabelName n) <+> (text $ unIdent i)
      ChiFun -> text "fun"
      ChiAny -> text "_"
    ) <+> text "->" <+> makeDoc e

instance Display Assignable where
  makeDoc (AIdent i) = makeDoc i
  makeDoc (ACell v) = makeDoc v

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
