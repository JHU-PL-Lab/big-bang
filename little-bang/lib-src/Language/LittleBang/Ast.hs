{-# LANGUAGE FlexibleInstances, EmptyDataDecls, GADTs, StandaloneDeriving #-}
module Language.LittleBang.Ast
( Expr(..)
, Chi(..)
, ChiMain
, ChiStruct
, ChiBind
, ChiPrimary
, Branches
, Branch(..)
-- Re-exported for convenience
, BinaryOperator(..)
, SubTerm(..)
) where

import Language.LittleBang.Types.UtilTypes
  ( LabelName
  , Ident
  , unIdent
  , unLabelName
  , BinaryOperator(..)
  , SubTerm(..)
  )
import qualified Language.LittleBang.Types.UtilTypes as T
  ( PrimitiveType(..) )
import Utils.Render.Display

-------------------------------------------------------------------------------

-- |Data type for representing Big Bang ASTs.
data Expr
  = Var Ident
  | Label LabelName Expr
  | Onion Expr Expr
  | OnionSub Expr SubTerm
  | EmptyOnion
  | Func Ident Expr
  | Appl Expr Expr
  | PrimInt Integer
  | PrimChar Char
  | PrimUnit
  | Case Expr Branches
  | Def Ident Expr Expr
  | Assign Ident Expr Expr
  | BinOp BinaryOperator Expr Expr
  | Self
  deriving (Eq, Ord, Show)

-- TODO: fix this boilerplate using -XDataKinds in ghc 7.4
data ChiMainType
data ChiStructType
data ChiBindType
data ChiPrimaryType

type ChiMain = Chi ChiMainType
type ChiStruct = Chi ChiStructType
type ChiBind = Chi ChiBindType
type ChiPrimary = Chi ChiPrimaryType

-- |Data type describing top level type patterns in case expressions;
--  corresponds to chi in the document.
data Chi a where
  ChiSimple       :: Maybe Ident -> ChiMain
  ChiComplex      :: ChiStruct   -> ChiMain

  ChiOnionOne     :: ChiBind              -> ChiStruct
  ChiOnionMany    :: ChiBind -> ChiStruct -> ChiStruct

  ChiParen        :: Maybe Ident -> ChiStruct  -> ChiBind
  ChiPrimary      :: Maybe Ident -> ChiPrimary -> ChiBind

  ChiPrim         :: T.PrimitiveType                -> ChiPrimary
  ChiLabelSimple  :: LabelName       -> Maybe Ident -> ChiPrimary
  ChiLabelComplex :: LabelName       -> ChiBind     -> ChiPrimary
  ChiFun          ::                                   ChiPrimary

deriving instance Show (Chi a)
deriving instance Eq (Chi a)
deriving instance Ord (Chi a)

-- |Alias for case branches
type Branches = [Branch]
data Branch = Branch ChiMain Expr
  deriving (Eq, Ord, Show)

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
    EmptyOnion -> text "(&)"
    BinOp op e1 e2 -> makeDoc op <+> makeDoc e1 <+> makeDoc e2
    Def i v e -> hsep [text "def", makeDoc i, text "=", makeDoc v, text "in", makeDoc e]
    Assign i v e -> hsep [makeDoc i, text "=", makeDoc v, text "in", makeDoc e]
    Self -> text "self"

instance Display Branch where
  makeDoc (Branch chi e) =
    makeDoc chi <+> text "->" <+> makeDoc e

instance Display (Chi a) where
  makeDoc chi =
    case chi of
      ChiSimple mx -> docDummyMx mx
      ChiComplex chiStruct -> makeDoc chiStruct
      ChiOnionOne chiBind -> makeDoc chiBind
      ChiOnionMany chiBind chiStruct ->
        makeDoc chiBind <+> text "&" <+> makeDoc chiStruct
      ChiParen mx chiStruct ->
        docPreMx mx <+> makeDoc chiStruct
      ChiPrimary mx chiPri ->
        docPreMx mx <+> makeDoc chiPri
      ChiPrim p -> makeDoc p
      ChiLabelSimple lbl mx -> makeDoc lbl <+> docDummyMx mx
      ChiLabelComplex lbl chiBind -> makeDoc lbl <+> makeDoc chiBind
      ChiFun -> text "fun"
    where docDummyMx mx = text $ maybe "_" unIdent mx
          docPreMx mx = maybe empty (text . (++":") . unIdent) mx

