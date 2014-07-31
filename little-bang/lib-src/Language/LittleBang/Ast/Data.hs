{-# LANGUAGE TemplateHaskell #-}

module Language.LittleBang.Ast.Data
( Expr(..)
, BinaryOperator(..)
, Param(..)
, Pattern(..)
, Arg(..)
, Var(..)
, LabelName(..)
, RecordTerm(..)
, RecordArgument(..)
, unLabelName
, PrimitiveType(..)
) where

import Control.Applicative

import Language.TinyBang.Ast.Origin
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.TemplateHaskell.Deriving

-- | AST structure for LittleBang.  This AST includes nodes for each of the
--   TinyBangNested AST nodes so that it may be used as both the source and
--   target of translation.
data Expr
  -- Constructors representing TBN nodes
  = TExprLet Origin Var Expr Expr
  | TExprScape Origin Pattern Expr
  | TExprBinaryOp Origin Expr BinaryOperator Expr
  | TExprOnion Origin Expr Expr
  | TExprAppl Origin Expr Expr
  | TExprLabelExp Origin LabelName Expr -- TODO: rename
  | TExprRef Origin Expr
  | TExprVar Origin Var
  | TExprValInt Origin Integer -- TODO: reorganize into TExprPrimLit or similar
  | TExprValEmptyOnion Origin 
  -- Constructors representing LB-specific nodes
  | LExprScape Origin [Param] Expr
  | LExprAppl Origin Expr [Arg]
  | LExprCondition Origin Expr Expr Expr
  | LExprSequence Origin Expr Expr -- TODO: shouldn't this just be a binop?
  | LExprList Origin [Expr]
  | LExprRecord Origin [RecordTerm]
  | LExprProjection Origin Expr Expr
  | LExprObject Origin [RecordTerm]
  deriving (Show)

data BinaryOperator
  = OpIntPlus Origin
  | OpIntMinus Origin
  | OpIntEq Origin 
  | OpIntGreaterEq Origin
  | OpIntLessEq Origin
  | OpSet Origin
  deriving (Show)

data Param
  = Param Origin Var Pattern
  deriving (Show)

-- TODO: probably need T and L prefixes on patterns too (like on Expr)
data Pattern
  = PrimitivePattern Origin PrimitiveType
  | LabelPattern Origin LabelName Pattern
  | RefPattern Origin Pattern
  | ConjunctionPattern Origin Pattern Pattern
  | EmptyPattern Origin
  | VariablePattern Origin Var
  -- LittleBang-specific
  | ListPattern Origin [Pattern] (Maybe Pattern)
  deriving (Show)

data Arg
  = PositionalArg Origin Expr
  | NamedArg Origin String Expr
  deriving (Show)

-- LittleBang object/record terms
data RecordTerm
  = TermIdent Origin LabelName Expr
  | TermScape Origin LabelName [RecordArgument] Expr
  | TermAnon Origin [RecordArgument] Expr
  | TermNativeExpr Origin Expr -- TODO is this too much of a hack maybe? Used as an adapter to Expr
  deriving (Show)

data RecordArgument
  = ArgIdent Origin LabelName
  | ArgPat Origin LabelName Pattern
  | ArgNativePat Origin Pattern -- TODO
  deriving (Show)

data Var
  = Var Origin String
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
      , ''RecordTerm
      , ''RecordArgument
      , ''Var
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
    LExprAppl orig _ _ -> orig
    LExprCondition orig _ _ _ -> orig
    LExprSequence orig _ _ -> orig
    LExprList orig _ -> orig
    LExprRecord orig _ -> orig
    LExprProjection orig _ _ -> orig
    LExprObject orig _ -> orig

instance HasOrigin Var where
  originOf x = case x of
    Var orig _ -> orig

instance HasOrigin Pattern where
  originOf x = case x of
    ConjunctionPattern orig _ _ -> orig
    LabelPattern orig _ _ -> orig
    RefPattern orig _ -> orig
    PrimitivePattern orig _ -> orig
    EmptyPattern orig -> orig
    VariablePattern orig _ -> orig
    ListPattern orig _ _ -> orig

instance HasOrigin RecordTerm where
  originOf x = case x of
   TermIdent orig _ _ -> orig
   TermScape orig _ _ _ -> orig
   TermAnon orig _ _ -> orig
   TermNativeExpr orig _ -> orig

instance HasOrigin RecordArgument where
  originOf x = case x of
   ArgIdent orig _ -> orig
   ArgPat orig _ _ -> orig
   ArgNativePat orig _  -> orig

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
   LExprAppl _ e args -> parens (makeDoc e) <+> encloseSep lparen rparen comma (map makeDoc args)
   LExprCondition _ e1 e2 e3 -> text "if" <+> makeDoc e1 <+> text "then" <+>
                                makeDoc e2 <+> text "else" <+> makeDoc e3
   LExprSequence _ e1 e2 -> makeDoc e1 <+> text "; " <+> makeDoc e2
   LExprList _ e -> text "[" <> foldl (<+>) (text "") (map makeDoc e) <> text "]"
   LExprRecord _ e -> text "record (" <> foldl (<+>) (text "") (map makeDoc e) <> text ")"
   LExprObject _ e -> text "object (" <> makeDoc e <> text ")"
   LExprProjection _ e1 e2 -> makeDoc e1 <> text "." <> makeDoc e2

instance Display BinaryOperator where
  makeDoc x = case x of
   OpIntPlus _ -> text "+"
   OpIntMinus _ -> text "-"
   OpIntEq _ -> text "=="
   OpIntGreaterEq _ -> text ">="
   OpIntLessEq _ -> text "<="
   OpSet _ -> text "<-"

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
    NamedArg _ name expr -> text name <> char '=' <> makeDoc expr

instance Display RecordTerm where
  makeDoc tm = case tm of
   TermIdent _ l e -> makeDoc l <> text "=" <> makeDoc e
   TermScape _ l e1 e2 -> makeDoc l <+> text "(" <> makeDoc e1 <> text ") =" <+> makeDoc e2
   TermAnon _ e1 e2 -> text "\\(" <> makeDoc e1 <> text ") =" <+> makeDoc e2
   TermNativeExpr _ e -> text "record from" <+> makeDoc e

instance Display RecordArgument where
  makeDoc arg = case arg of
   ArgIdent _ l -> makeDoc l
   ArgPat _ l p -> makeDoc l <> text ":" <> makeDoc p
   ArgNativePat _ p -> text "arg pat from" <+> makeDoc p

instance Display Var where
  makeDoc x = case x of
    Var _ i -> text i

instance Display LabelName where
  makeDoc x = case x of
    LabelName _ l -> text $ '`' : l

instance Display PrimitiveType where
  makeDoc p = case p of
    PrimInt -> text "int"
