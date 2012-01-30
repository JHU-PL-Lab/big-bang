{- |A module containing Haskell types which are used as utilities by other
    modules.
-}
module Language.MicroBang.Types.UtilTypes
( LabelName
, labelName
, unLabelName
, Ident
, ident
, unIdent
, LazyOperator(..)
, EagerOperator(..)
, SubTerm(..)
, PrimitiveType(..)
) where

import Utils.Render.Display

-- |A distinguished type for labels.
newtype LabelName = LabelName { unLabelName :: String }
    deriving (Eq, Ord, Show)
labelName :: String -> LabelName
{- TODO: smarter constructor -}
labelName s = LabelName s

-- |A distinguished type for identifiers.
newtype Ident = Ident { unIdent :: String }
    deriving (Eq, Ord, Show)
ident :: String -> Ident
{- TODO: smarter constructor -}
ident s = Ident s

instance Display LabelName where
    makeDoc = text . unLabelName

instance Display Ident where
    makeDoc = text . unIdent

-- |Data type used to represent lazy operations
data LazyOperator
  = Plus
  | Minus
  deriving (Eq, Ord, Show)

-- |Data type used to represent eager operations
data EagerOperator
  = Equal
  | LessEqual
  | GreaterEqual
  deriving (Eq, Ord, Show)

instance Display LazyOperator where
  makeDoc o = brackets . text $
    case o of
      Plus -> "+"
      Minus -> "-"

instance Display EagerOperator where
  makeDoc o = brackets $ text $
    case o of
      Equal -> "="
      LessEqual -> "<="
      GreaterEqual -> ">="

-- |The datatype enumerating the primitives in the Little Bang type system.
data PrimitiveType
  = PrimInt
  | PrimChar
  | PrimUnit
  deriving (Eq, Ord, Show)

instance Display PrimitiveType where
  makeDoc p =
    case p of
      PrimInt -> text "int"
      PrimChar -> text "char"
      PrimUnit -> text "unit"

-- |Data type describing type patterns for removing values from onions.
data SubTerm
  = SubPrim PrimitiveType
  | SubLabel LabelName
  | SubFunc
  deriving (Eq, Ord, Show)

instance Display SubTerm where
  makeDoc a = text "-" <> case a of
    SubPrim p -> makeDoc p
    SubLabel n -> makeDoc n
    SubFunc -> text "fun"
