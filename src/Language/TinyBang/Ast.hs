module Language.TinyBang.Ast
( Expr(..)
, Chi(..)
, Branches
, Branch(..)
, Value(..)
-- Re-exported for convenience
, LazyOperator(..)
, EagerOperator(..)
, exprFromValue
) where

import Language.TinyBang.Render.Display
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.UtilTypes
  ( LabelName
  , Ident
  , unIdent
  , unLabelName
  , LazyOperator(..)
  , EagerOperator(..)
  )

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
    | LazyOp Expr LazyOperator Expr
    | EagerOp Expr EagerOperator Expr
    deriving (Eq, Ord, Show)

-- |Data type for representing Big Bang values
data Value =
      VLabel LabelName Value
    | VOnion Value Value
    | VFunc Ident Expr
    | VPrimInt Integer
    | VPrimChar Char
    | VPrimUnit
    deriving (Eq, Ord, Show)

-- |Data type describing type patterns for case expressions.
data Chi =
      ChiPrim (T.PrimitiveType)
    | ChiLabel LabelName Ident
    | ChiFun
    | ChiAny
    deriving (Eq, Ord, Show)

-- |Alias for case branches
type Branches = [Branch]
data Branch = Branch (Maybe Ident) Chi Expr
    deriving (Eq, Ord, Show)

-- |Trivial conversion from values to exprs
exprFromValue :: Value -> Expr
exprFromValue v = case v of
  VLabel l v1  -> Label l $ exprFromValue v1
  VOnion v1 v2 -> Onion (exprFromValue v1) (exprFromValue v2)
  VFunc i e    -> Func i e
  VPrimInt i   -> PrimInt i
  VPrimChar c  -> PrimChar c
  VPrimUnit    -> PrimUnit

instance Display Expr where
    makeDoc a = case a of
        Var i -> text $ unIdent i
        Label n e -> char '`' <> (text $ unLabelName n) <+> makeDoc e
        Onion e1 e2 -> makeDoc e1 <+> char '&' <+> makeDoc e2
        Func i e -> parens $
                text "fun" <+> (text $ unIdent i) <+> text "->" <+> makeDoc e
        Appl e1 e2 -> makeDoc e1 <+> makeDoc e2
        PrimInt i -> integer i
        PrimChar c -> quotes $ char c
        PrimUnit -> parens empty
        Case e brs -> text "case" <+> makeDoc e <+> text "of" <+> text "{" $+$
                (nest indentSize $ vcat $ punctuate semi $ map makeDoc brs)
                $+$ text "}"
        LazyOp e1 op e2 -> makeDoc e1 <+> makeDoc op <+> makeDoc e2
        EagerOp e1 op e2 -> makeDoc e1 <+> makeDoc op <+> makeDoc e2
        {-
           TODO: deal with the fact that the following isn't actually code
           options include:
               * errors on ASTs containing builtin nodes
               * namespacing trick (e.g., Plus translates to
                   "Language.TinyBang.Builtins.([+]) e1 e2"
        -}

instance Display Value where
  makeDoc = makeDoc . exprFromValue

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