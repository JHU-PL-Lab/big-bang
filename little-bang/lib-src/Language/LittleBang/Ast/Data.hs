{-# LANGUAGE TemplateHaskell #-}

module Language.LittleBang.Ast.Data
( Expr(..)
, BinaryOperator(..)
, Pattern(..)
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

-- | AST structure for LittleBang

data Expr
  -- Carryovers from TinyBangNested
  = ExprLet Origin Var Expr Expr
  | ExprScape Origin Pattern Expr
  | ExprBinaryOp Origin Expr BinaryOperator Expr
  | ExprOnion Origin Expr Expr
  | ExprAppl Origin Expr Expr
  | ExprLabelExp Origin LabelName Expr
  | ExprRef Origin Expr
  | ExprVar Origin Var
  | ExprValInt Origin Integer
  | ExprValEmptyOnion Origin 
  -- LittleBang-specific
  | ExprCondition Origin Expr Expr Expr
  | ExprSequence Origin Expr Expr
  | ExprList Origin [Expr]
  | ExprRecord Origin [RecordTerm]
  | ExprProjection Origin Expr Expr
  | ExprObject Origin [RecordTerm]
  deriving (Show)

data BinaryOperator
  = OpIntPlus Origin
  | OpIntMinus Origin
  | OpIntEq Origin 
  | OpIntGreaterEq Origin
  | OpIntLessEq Origin
  | OpSet Origin
  deriving (Show)

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
      , ''Pattern
      , ''RecordTerm
      , ''RecordArgument
      , ''Var
      , ''LabelName
      ]
  ])

-- | HasOrigin instances for Expr 

instance HasOrigin Expr where
  originOf x = case x of
    ExprLet orig _ _ _ -> orig
    ExprScape orig _ _ -> orig
    ExprBinaryOp orig _ _ _ -> orig
    ExprOnion orig _ _ -> orig
    ExprAppl orig _ _ -> orig
    ExprLabelExp orig _ _-> orig
    ExprVar orig _ -> orig
    ExprValInt orig _ -> orig
    ExprValEmptyOnion orig -> orig
    ExprCondition orig _ _ _ -> orig
    ExprRef orig _ -> orig
    ExprSequence orig _ _ -> orig
    ExprList orig _ -> orig
    ExprRecord orig _ -> orig

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
   ExprLet _ v e1 e2 -> text "let " <> makeDoc v <> text " = (" <> makeDoc e1 <> text ") in (" <> makeDoc e2 <> text ")"
   ExprScape _ op e -> text "(" <> makeDoc op <> text ") -> (" <> makeDoc e <> text ")"
   ExprBinaryOp _ e1 ao e2 -> text "(" <> makeDoc e1 <> text ") " <> makeDoc ao <> text " (" <> makeDoc e2 <> text ")"
   ExprOnion _ e1 e2 -> text "(" <> makeDoc e1 <> text ") & (" <> makeDoc e2 <> text ")"
   ExprAppl _ e1 e2 -> text "(" <> makeDoc e1 <> text ") (" <> makeDoc e2 <> text ")"
   ExprLabelExp _ l e -> text "(" <> makeDoc l <+> makeDoc e <> text ")"
   ExprRef _ e -> text "ref " <> makeDoc e
   ExprVar _ v -> makeDoc v 
   ExprValInt _ i -> text $ show i
   ExprValEmptyOnion _ -> text "()"
   ExprCondition _ e1 e2 e3 -> text "if" <+> makeDoc e1 <+> text "then" <+>
                               makeDoc e2 <+> text "else" <+> makeDoc e3
   ExprSequence _ e1 e2 -> makeDoc e1 <+> text "; " <+> makeDoc e2
   ExprList _ e -> text "[" <> foldl (<+>) (text "") (map makeDoc e) <> text "]"
   ExprRecord _ e -> text "record (" <> foldl (<+>) (text "") (map makeDoc e) <> text ")"
   ExprObject _ e -> text "object (" <> makeDoc e <> text ")"
   ExprProjection _ e1 e2 -> makeDoc e1 <> text "." <> makeDoc e2

instance Display BinaryOperator where
  makeDoc x = case x of
   OpIntPlus _ -> text "+"
   OpIntMinus _ -> text "-"
   OpIntEq _ -> text "=="
   OpIntGreaterEq _ -> text ">="
   OpIntLessEq _ -> text "<="
   OpSet _ -> text "<-"

instance Display Pattern where
  makeDoc pat = case pat of
   PrimitivePattern _ prim -> makeDoc prim
   LabelPattern _ l p -> text "(" <> makeDoc l <+> makeDoc p <> text ")"
   RefPattern _ p -> text "ref" <+> parens (makeDoc p )
   ConjunctionPattern _ p1 p2 ->  text "(" <> makeDoc p1  <+> text "&pat" <+> makeDoc p2 <> text ")"
   EmptyPattern _ -> text "()"
   VariablePattern _ x -> makeDoc x
   ListPattern _ p _ -> text "[" <> foldl (<+>) (text "") (map makeDoc p) <> text "]"  -- TODO: include ... form

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
    LabelName _ l -> text $ "`" ++ l

instance Display PrimitiveType where
  makeDoc p = case p of
    PrimInt -> text "int"
