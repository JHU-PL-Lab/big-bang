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
, Modifier(..)
, exprVars
-- Re-exported for convenience
, BinaryOperator(..)
, ProjTerm(..)
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.LittleBang.Types.UtilTypes
  ( LabelName
  , Ident
  , unIdent
  , unLabelName
  , BinaryOperator(..)
  , ProjTerm(..)
  )
import qualified Language.LittleBang.Types.UtilTypes as T
  ( PrimitiveType(..) )
import Utils.Render.Display

-------------------------------------------------------------------------------

-- |Data type for representing Big Bang ASTs.
data Expr
  = Var Ident
  | Label LabelName (Maybe Modifier) Expr
  | Onion Expr Expr
  | OnionSub Expr ProjTerm
  | OnionProj Expr ProjTerm
  | EmptyOnion
  | Func Ident Expr
  | Appl Expr Expr
  | PrimInt Integer
  | PrimChar Char
  | PrimUnit
  | Case Expr Branches
  | Def (Maybe Modifier) Ident Expr Expr
  | Assign Ident Expr Expr
  | BinOp BinaryOperator Expr Expr
  | Self
  | Prior
  | Proj Expr Ident
  | ProjAssign Expr Ident Expr Expr
  deriving (Eq, Ord, Show)

data Modifier
  = Final
  | Immutable
  deriving (Eq, Ord, Show, Enum)

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
  ChiTopVar       :: Ident                   -> ChiMain
  ChiTopOnion     :: ChiPrimary -> ChiStruct -> ChiMain
  ChiTopBind      :: ChiBind                 -> ChiMain

  ChiOnionMany    :: ChiPrimary -> ChiStruct -> ChiStruct
  ChiOnionOne     :: ChiPrimary              -> ChiStruct

  ChiBound        :: Ident -> ChiBind -> ChiBind
  ChiUnbound      :: ChiPrimary       -> ChiBind  

  ChiPrim         :: T.PrimitiveType                -> ChiPrimary
  ChiLabelShallow :: LabelName       -> Ident       -> ChiPrimary
  ChiLabelDeep    :: LabelName       -> ChiBind     -> ChiPrimary
  ChiFun          ::                                   ChiPrimary
  ChiInnerStruct  :: ChiStruct                      -> ChiPrimary

deriving instance Show (Chi a)
deriving instance Eq (Chi a)
deriving instance Ord (Chi a)

-- |Alias for case branches
type Branches = [Branch]
data Branch = Branch ChiMain Expr
  deriving (Eq, Ord, Show)

exprVars :: Expr -> Set Ident
exprVars e =
  case e of
    Var i -> Set.singleton i
    Label _ _ e' -> exprVars e'
    Onion e1 e2 -> exprVars e1 `Set.union` exprVars e2
    Func i e' -> Set.insert i $ exprVars e'
    Appl e1 e2 -> exprVars e1 `Set.union` exprVars e2
    PrimInt _ -> Set.empty
    PrimChar _ -> Set.empty
    PrimUnit -> Set.empty
    Case e' brs -> Set.union (exprVars e') $ Set.unions $ map branchVars brs
    OnionSub e' _ -> exprVars e'
    OnionProj e' _ -> exprVars e'
    EmptyOnion -> Set.empty
    BinOp _ e1 e2 -> exprVars e1 `Set.union` exprVars e2
    Def _ i e1 e2 -> Set.insert i $ exprVars e1 `Set.union` exprVars e2
    Assign i e1 e2 -> Set.insert i $ exprVars e1 `Set.union` exprVars e2
    Self -> Set.empty
    Prior -> Set.empty
    Proj e' i -> Set.insert i $ exprVars e'
    ProjAssign e1 i e2 e3 ->
      Set.insert i $ Set.unions $ map exprVars [e1,e2,e3]
  where branchVars (Branch chi e') = patVars chi `Set.union` exprVars e'

patVars :: Chi a -> Set Ident
patVars chi =
  case chi of
    ChiTopVar i -> Set.singleton i
    ChiTopOnion p s -> patVars p `Set.union` patVars s
    ChiTopBind b -> patVars b
    ChiOnionMany p s -> patVars p `Set.union` patVars s
    ChiOnionOne p -> patVars p
    ChiBound i b -> Set.insert i $ patVars b
    ChiUnbound p -> patVars p
    ChiPrim _ -> Set.empty
    ChiLabelShallow _ i -> Set.singleton i
    ChiLabelDeep _ b -> patVars b
    ChiFun -> Set.empty
    ChiInnerStruct s -> patVars s

instance Display Expr where
  makeDoc a = case a of
    Var i -> text $ unIdent i
    Label n m e -> char '`' <> (text $ unLabelName n) <+> dispMod m <+> makeDoc e
    Onion e1 e2 -> makeDoc e1 <+> char '&' <+> makeDoc e2
    Func i e -> parens $
            text "fun" <+> (text $ unIdent i) <+> text "->" <+> makeDoc e
    Appl e1 e2 -> parens $ makeDoc e1 <+> makeDoc e2
    PrimInt i -> integer i
    PrimChar c -> quotes $ char c
    PrimUnit -> parens empty
    Case e brs -> text "case" <+> (parens $ makeDoc e) <+> text "of"
            <+> text "{" $+$
            (nest indentSize $ vcat $ punctuate semi $ map makeDoc brs)
            $+$ text "}"
    OnionSub e p -> makeDoc e <+> text "&-" <+> makeDoc p
    OnionProj e p -> makeDoc e <+> text "&." <+> makeDoc p
    EmptyOnion -> text "(&)"
    BinOp op e1 e2 -> makeDoc op <+> makeDoc e1 <+> makeDoc e2
    Def m i v e -> hsep [text "def", dispMod m, makeDoc i,
                         text "=", makeDoc v, text "in", makeDoc e]
    Assign i v e -> hsep [makeDoc i, text "=", makeDoc v, text "in", makeDoc e]
    Self -> text "self"
    Prior -> text "prior"
    Proj e i -> makeDoc e <> text "." <> makeDoc i
    ProjAssign e i e' e'' -> makeDoc e <> text "." <> makeDoc i <+> text "="
                                       <+> makeDoc e' <+> text "in" <+> makeDoc e''
    where dispMod m = case m of
            Just Final -> text "final"
            Just Immutable -> text "immut"
            Nothing -> empty

instance Display Branch where
  makeDoc (Branch chi e) =
    makeDoc chi <+> text "->" <+> makeDoc e

instance Display (Chi a) where
  makeDoc chi =
    case chi of
      ChiTopVar x -> iDoc x
      ChiTopOnion p s -> makeDoc p <+> text "&" <+> makeDoc s
      ChiTopBind b -> makeDoc b
      ChiOnionMany p s -> makeDoc p <+> text "&" <+> makeDoc s
      ChiOnionOne p -> makeDoc p
      ChiBound i b -> iDoc i <> text ":" <> makeDoc b
      ChiUnbound p -> makeDoc p
      ChiPrim p -> makeDoc p
      ChiLabelShallow lbl x -> makeDoc lbl <+> iDoc x 
      ChiLabelDeep lbl b -> makeDoc lbl <+> makeDoc b
      ChiFun -> text "fun"
      ChiInnerStruct s -> parens $ makeDoc s
    where iDoc = text . unIdent
