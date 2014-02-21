module Language.LittleBang.Ast.Data
( Expr(..)
, BinaryOperator(..)
, Pattern(..)
, Var(..)
, LabelName(..)
, unLabelName
, PrimitiveType(..)
) where

-- Haskell module generated by the BNF converter

import Language.TinyBang.Ast.Origin
import Language.TinyBang.Utils.Display

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
  | ExprChain Origin Expr Expr
  deriving (Eq,Ord,Show)

data BinaryOperator
  = OpIntPlus Origin
  | OpIntMinus Origin
  | OpIntEq Origin 
  | OpIntGreaterEq Origin
  | OpIntLessEq Origin
  deriving (Eq,Ord,Show)

data Pattern
  = PrimitivePattern Origin PrimitiveType
  | LabelPattern Origin LabelName Pattern
  | ConjunctionPattern Origin Pattern Pattern
  | EmptyPattern Origin
  | VariablePattern Origin Var
  deriving (Eq,Ord,Show)

data Var
  = Var Origin String
  deriving (Eq,Ord,Show)

data LabelName
  = LabelName Origin String
  deriving (Eq,Ord,Show)
  
unLabelName :: LabelName -> String
unLabelName (LabelName _ s) = s

data PrimitiveType
  = PrimInt
  deriving (Eq,Ord,Show)

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

instance HasOrigin Var where
  originOf x = case x of
    Var orig _ -> orig

instance HasOrigin Pattern where
  originOf x = case x of
   ConjunctionPattern orig _ _ -> orig
   LabelPattern orig _ _ -> orig
   PrimitivePattern orig _ -> orig
   EmptyPattern orig -> orig
   VariablePattern orig _ -> orig

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
   ExprAppl _ e1 e2 -> text "(" <> makeDoc e1 <> text ") apply (" <> makeDoc e2 <> text ")"
   ExprLabelExp _ l e -> text "(" <> makeDoc l <+> makeDoc e <> text ")"
   ExprVar _ v -> makeDoc v 
   ExprValInt _ i -> text $ show i
   ExprValEmptyOnion _ -> text "()"
   ExprCondition _ e1 e2 e3 -> text "if" <+> makeDoc e1 <+> text "then" <+>
                               makeDoc e2 <+> text "else" <+> makeDoc e3

instance Display BinaryOperator where
  makeDoc x = case x of
   OpIntPlus _ -> text "+"
   OpIntMinus _ -> text "-"
   OpIntEq _ -> text "=="
   OpIntGreaterEq _ -> text ">="
   OpIntLessEq _ -> text "<="

instance Display Pattern where
  makeDoc pat = case pat of
   PrimitivePattern _ prim -> makeDoc prim
   LabelPattern _ l p -> text "(" <> makeDoc l <+> makeDoc p <> text ")" 
   ConjunctionPattern _ p1 p2 ->  text "(" <> makeDoc p1  <+> text "&pat" <+> makeDoc p2 <> text ")"
   EmptyPattern _ -> text "()"
   VariablePattern _ x -> makeDoc x

instance Display Var where
  makeDoc x = case x of
    Var _ i -> text i

instance Display LabelName where
  makeDoc x = case x of
    LabelName _ l -> text $ "`" ++ l

instance Display PrimitiveType where
  makeDoc p = case p of
    PrimInt -> text "int"
