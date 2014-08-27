{-# LANGUAGE MultiParamTypeClasses #-}
{-|
  A simple module which converts TBN ASTs into their LB equivalents.
-}
module Language.LittleBang.Lifter
( TBNLiftable(..)
) where

import Language.LittleBang.Ast as LB
import Language.TinyBang.Ast
import Language.TinyBangNested.Ast as TBN

class TBNLiftable a b where
  tbnLift :: a -> b

instance TBNLiftable TBN.Expr LB.Expr where
  tbnLift expr = case expr of
    TBN.ExprLet o i e1 e2 -> ol3 LB.TExprLet o i e1 e2
    TBN.ExprScape o p e -> ol2 LB.TExprScape o p e
    TBN.ExprBinaryOp o e1 op e2 -> ol3 LB.TExprBinaryOp o e1 op e2
    TBN.ExprOnion o e1 e2 -> ol2 LB.TExprOnion o e1 e2
    TBN.ExprAppl o e1 e2 -> ol2 LB.TExprAppl o e1 e2
    TBN.ExprLabelExp o n e -> ol2 LB.TExprLabelExp o n e
    TBN.ExprRef o e -> ol1 LB.TExprRef o e
    TBN.ExprVar o i -> ol1 LB.TExprVar o i
    TBN.ExprValInt o i -> LB.TExprValInt o i
    TBN.ExprValEmptyOnion o -> LB.TExprValEmptyOnion o

instance TBNLiftable TBN.Pattern LB.Pattern where
  tbnLift pat = case pat of
    TBN.PrimitivePattern o pt -> ol1 LB.PrimitivePattern o pt
    TBN.LabelPattern o n p -> ol2 LB.LabelPattern o n p
    TBN.RefPattern o p -> ol1 LB.RefPattern o p
    TBN.ConjunctionPattern o p1 p2 -> ol2 LB.ConjunctionPattern o p1 p2
    TBN.EmptyPattern o -> LB.EmptyPattern o
    TBN.VariablePattern o i -> ol1 LB.VariablePattern o i

instance TBNLiftable TBN.LabelName LB.LabelName where
  tbnLift (TBN.LabelName o n) = LB.LabelName o n

instance TBNLiftable TBN.Ident LB.Ident where
  tbnLift (TBN.Ident o s) = LB.Ident o s
  
instance TBNLiftable TBN.BinaryOperator TBN.BinaryOperator where -- TODO: remove call to this function if possible.
  tbnLift op = case op of
    _ -> op
{-
    TBN.OpIntPlus o -> LB.OpIntPlus o
    TBN.OpIntMinus o -> LB.OpIntMinus o
    TBN.OpIntEq o -> LB.OpIntEq o
    TBN.OpIntGreaterEq o -> LB.OpIntGreaterEq o
    TBN.OpIntLessEq o -> LB.OpIntLessEq o
    TBN.OpSet o -> LB.OpSet o
-}
instance TBNLiftable TBN.PrimitiveType LB.PrimitiveType where
  tbnLift pt = case pt of
    TBN.PrimInt -> LB.PrimInt
    TBN.PrimChar -> LB.PrimChar

ol1 :: (TBNLiftable a a') => (Origin -> a' -> r) -> Origin -> a -> r
ol1 f o a = f o (tbnLift a)

ol2 :: (TBNLiftable a a', TBNLiftable b b')
    => (Origin -> a' -> b' -> r) -> Origin -> a -> b -> r
ol2 f o a b = f o (tbnLift a) (tbnLift b)

ol3 :: (TBNLiftable a a', TBNLiftable b b', TBNLiftable c c')
    => (Origin -> a' -> b' -> c' -> r) -> Origin -> a -> b -> c -> r
ol3 f o a b c = f o (tbnLift a) (tbnLift b) (tbnLift c)

