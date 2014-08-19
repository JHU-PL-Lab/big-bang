{-# LANGUAGE TemplateHaskell #-}

module Language.LittleBang.Ast.Data
( Expr(..)
, BinaryOperator(..)
, Param(..)
, Pattern(..)
, Arg(..)
, Ident(..)
, LabelName(..)
, ObjectTerm(..)
, unLabelName
, PrimitiveType(..)
) where

import Control.Applicative

import Language.TinyBang.Ast.Origin
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.TemplateHaskell.Deriving
import qualified Language.TinyBangNested.Ast as TBN

-- | AST structure for LittleBang.  This AST includes nodes for each of the
--   TinyBangNested AST nodes so that it may be used as both the source and
--   target of translation.
data Expr
  -- Constructors representing TBN nodes
  = TExprLet Origin Ident Expr Expr
  | TExprScape Origin Pattern Expr
  | TExprBinaryOp Origin Expr TBN.BinaryOperator Expr
  | TExprOnion Origin Expr Expr
  | TExprAppl Origin Expr Expr
  | TExprLabelExp Origin LabelName Expr -- TODO: rename
  | TExprRef Origin Expr
  | TExprVar Origin Ident
  | TExprValInt Origin Integer -- TODO: reorganize into TExprPrimLit or similar
  | TExprValEmptyOnion Origin 
  -- Constructors representing LB-specific nodes
  | LExprScape Origin [Param] Expr
  | LExprBinaryOp Origin Expr BinaryOperator Expr
  | LExprAppl Origin Expr [Arg]
  | LExprCondition Origin Expr Expr Expr
  | LExprList Origin [Expr]
  | LExprRecord Origin [Arg]
  | LExprProjection Origin Expr Ident
  | LExprDispatch Origin Expr Ident [Arg]
  | LExprObject Origin [ObjectTerm]
  | LExprDeref Origin Expr
  | LExprIndexedList Origin Expr Expr
  deriving (Show)

data BinaryOperator
  = OpSeq Origin
  | OpCons Origin
  deriving (Show)

data Param
  = Param Origin Ident Pattern
  deriving (Show)

-- TODO: probably need T and L prefixes on patterns too (like on Expr)
data Pattern
  = PrimitivePattern Origin PrimitiveType
  | LabelPattern Origin LabelName Pattern
  | RefPattern Origin Pattern
  | ConjunctionPattern Origin Pattern Pattern
  | EmptyPattern Origin
  | VariablePattern Origin Ident
  -- LittleBang-specific
  | ListPattern Origin [Pattern] (Maybe Pattern)
  deriving (Show)

data Arg
  = PositionalArg Origin Expr
  | NamedArg Origin Ident Expr
  deriving (Show)
  
-- LittleBang object terms
data ObjectTerm
  = ObjectMethod Origin Ident [Param] Expr
  | ObjectField Origin Ident Expr
  deriving (Show)

data Ident
  = Ident Origin String
  deriving (Show)
  
data LabelName
  = LabelName Origin String
  deriving (Show)
  
unLabelName :: LabelName -> String
unLabelName (LabelName _ s) = s

data PrimitiveType
  = PrimInt
  deriving (Eq,Ord,Show)

-- |Generate Eq and Ord instances
  
$(concat <$> sequence
  [ f name
  | f <- [deriveEqSkipFirst, deriveOrdSkipFirst]
  , name <-
      [ ''Expr
      , ''BinaryOperator
      , ''Param
      , ''Pattern
      , ''Arg
      , ''ObjectTerm
      , ''Ident
      , ''LabelName
      ]
  ])

-- | HasOrigin instances for Expr 

instance HasOrigin Expr where
  originOf x = case x of
    TExprLet orig _ _ _ -> orig
    TExprScape orig _ _ -> orig
    TExprBinaryOp orig _ _ _ -> orig
    TExprOnion orig _ _ -> orig
    TExprAppl orig _ _ -> orig
    TExprLabelExp orig _ _-> orig
    TExprRef orig _ -> orig
    TExprVar orig _ -> orig
    TExprValInt orig _ -> orig
    TExprValEmptyOnion orig -> orig
    LExprScape orig _ _ -> orig
    LExprBinaryOp orig _ _ _ -> orig
    LExprAppl orig _ _ -> orig
    LExprCondition orig _ _ _ -> orig
    LExprList orig _ -> orig
    LExprRecord orig _ -> orig
    LExprProjection orig _ _ -> orig
    LExprDispatch orig _ _ _ -> orig
    LExprObject orig _ -> orig
    LExprDeref orig _ -> orig
    LExprIndexedList orig _ _ -> orig

instance HasOrigin Ident where
  originOf x = case x of
    Ident orig _ -> orig

instance HasOrigin Pattern where
  originOf x = case x of
    ConjunctionPattern orig _ _ -> orig
    LabelPattern orig _ _ -> orig
    RefPattern orig _ -> orig
    PrimitivePattern orig _ -> orig
    EmptyPattern orig -> orig
    VariablePattern orig _ -> orig
    ListPattern orig _ _ -> orig

instance HasOrigin ObjectTerm where
  originOf x = case x of
    ObjectMethod orig _ _ _ -> orig
    ObjectField orig _ _ -> orig

instance HasOrigin LabelName where
  originOf x = case x of
    LabelName orig _ -> orig

-- | Display instances for Expr 

instance Display Expr where
  makeDoc x = case x of
   TExprLet _ v e1 e2 -> text "let" <+> makeDoc v <+> text "=" <+> parens (makeDoc e1) <+> text "in" <+> parens (makeDoc e2)
   TExprScape _ op e -> text "#TScape" <+> parens (makeDoc op) <+> text "->" <+> parens (makeDoc e)
   TExprBinaryOp _ e1 ao e2 -> parens (makeDoc e1) <+> makeDoc ao <+> parens (makeDoc e2)
   TExprOnion _ e1 e2 -> parens (makeDoc e1) <+> text "&" <+> parens (makeDoc e2)
   TExprAppl _ e1 e2 -> text "#TAppl" <+> parens (makeDoc e1) <+> parens (makeDoc e2)
   TExprLabelExp _ l e -> parens (makeDoc l <+> makeDoc e)
   TExprRef _ e -> text "ref" <+> makeDoc e
   TExprVar _ v -> makeDoc v 
   TExprValInt _ i -> text $ show i
   TExprValEmptyOnion _ -> text "()"
   LExprScape _ op e -> parens (makeDoc op) <+> text "->" <+> parens (makeDoc e)
   LExprBinaryOp _ e1 ao e2 -> parens (makeDoc e1) <+> makeDoc ao <+> parens (makeDoc e2)
   LExprAppl _ e args -> parens (makeDoc e) <+> encloseSep lparen rparen comma (map makeDoc args)
   LExprCondition _ e1 e2 e3 -> text "if" <+> makeDoc e1 <+> text "then" <+>
                                makeDoc e2 <+> text "else" <+> makeDoc e3
   LExprList _ e -> text "[" <> foldl (<+>) (text "") (map makeDoc e) <> text "]"
   LExprRecord _ args ->
    encloseSep lbrace rbrace comma $ map makeDoc args
   LExprObject _ terms ->
    text "object" <+> encloseSep lbrace rbrace comma (map makeDoc terms)
   LExprProjection _ e i -> makeDoc e <> text "." <> makeDoc i
   LExprDispatch _ e i a -> makeDoc e <> text "." <> makeDoc i <>
                              encloseSep lparen rparen comma (map makeDoc a)
   LExprDeref _ e -> text "!" <> makeDoc e
   LExprIndexedList _ e i -> makeDoc e <> text "[" <> makeDoc i <> text "]"

instance Display BinaryOperator where
  makeDoc x = case x of
   OpSeq _ -> text ";"
   OpCons _ -> text "::"

instance Display Param where
  makeDoc param = case param of
    Param _ v pat -> makeDoc v <> char ':' <> makeDoc pat

instance Display Pattern where
  makeDoc pat = case pat of
   PrimitivePattern _ prim -> makeDoc prim
   LabelPattern _ l p -> text "(" <> makeDoc l <+> makeDoc p <> text ")"
   RefPattern _ p -> text "ref" <+> parens (makeDoc p )
   ConjunctionPattern _ p1 p2 ->  text "(" <> makeDoc p1  <+> text "&pat" <+> makeDoc p2 <> text ")"
   EmptyPattern _ -> text "()"
   VariablePattern _ x -> makeDoc x
   ListPattern _ p _ -> text "[" <> foldl (<+>) (text "") (map makeDoc p) <> text "]"  -- TODO: include ... form

instance Display Arg where
  makeDoc arg = case arg of
    PositionalArg _ expr -> makeDoc expr
    NamedArg _ name expr -> makeDoc name <> char '=' <> makeDoc expr

instance Display ObjectTerm where
  makeDoc tm = case tm of
    ObjectMethod _ n params e ->
      text "method" <+> makeDoc n <>
      encloseSep lparen rparen comma (map makeDoc params) <+> text "=" <+>
      makeDoc e
    ObjectField _ n e ->
      makeDoc n <+> text "=" <+> makeDoc e

instance Display Ident where
  makeDoc x = case x of
    Ident _ i -> text i

instance Display LabelName where
  makeDoc x = case x of
    LabelName _ l -> text $ '`' : l

instance Display PrimitiveType where
  makeDoc p = case p of
    PrimInt -> text "int"
