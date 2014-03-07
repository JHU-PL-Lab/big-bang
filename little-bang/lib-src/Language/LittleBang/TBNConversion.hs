{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Language.LittleBang.TBNConversion
( convertToTBNExpr
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBangNested.Ast as TBN
import Control.Applicative

type TBNConvertError = String
type TBNConvertM = Either TBNConvertError

-- | Convert a LittleBang expression to a TinyBang Nested expression 
convertToTBNExpr :: LB.Expr -> Either TBNConvertError TBN.Expr
convertToTBNExpr expr = runTBNConvertM $ toTBN expr

runTBNConvertM :: TBNConvertM a -> Either TBNConvertError a
runTBNConvertM = id

class TBNConvertible a b | a -> b where
  toTBN :: a -> TBNConvertM b

-- | Convert a LittleBang expression to a TinyBang Nested expression
instance TBNConvertible LB.Expr TBN.Expr where
  toTBN expr = case expr of 
    LB.ExprCondition _ _ _ _ ->
      undefined -- TODO: generate correct error for untranslated node
    LB.ExprSequence _ _ _ ->
      undefined
    LB.ExprList _ _ ->
      undefined
    
    LB.ExprLet o var e1 e2 -> TBN.ExprLet o 
                                    <$> toTBN var 
                                    <*> toTBN e1 
                                    <*> toTBN e2
                                    
    LB.ExprScape o outerPattern e -> TBN.ExprScape o 
                                    <$> toTBN outerPattern 
                                    <*> toTBN e
                                    
    LB.ExprBinaryOp o e1 op e2 -> TBN.ExprBinaryOp o 
                                    <$> toTBN e1 
                                    <*> toTBN op
                                    <*> toTBN e2
                                    
    LB.ExprOnion o e1 e2 -> TBN.ExprOnion o 
                                    <$> toTBN e1 
                                    <*> toTBN e2
                                    
    LB.ExprAppl o e1 e2 -> TBN.ExprAppl o 
                                    <$> toTBN e1 
                                    <*> toTBN e2
                                    
    LB.ExprLabelExp o label e1 -> TBN.ExprLabelExp o 
                                    <$> toTBN label 
                                    <*> toTBN e1

    LB.ExprRef o e1 -> TBN.ExprRef o <$> toTBN e1
                                    
    LB.ExprVar o var -> TBN.ExprVar o <$> toTBN var
    LB.ExprValInt o int -> return $ TBN.ExprValInt o int
    LB.ExprValEmptyOnion o -> return $ TBN.ExprValEmptyOnion o
        
-- | Convert a LittleBang pattern to a TinyBang Nested pattern         
instance TBNConvertible LB.Pattern TBN.Pattern where
  toTBN pattern = case pattern of   
        LB.ListPattern _ _ _ ->
          undefined
        LB.PrimitivePattern o primitive ->
          TBN.PrimitivePattern o <$> toTBN primitive
        LB.LabelPattern o label p ->
          TBN.LabelPattern o <$> toTBN label <*> toTBN p
        LB.RefPattern o p ->
          TBN.RefPattern o <$> toTBN p
        LB.ConjunctionPattern o p1 p2 ->
          TBN.ConjunctionPattern o <$> toTBN p1 <*> toTBN p2
        LB.EmptyPattern o -> return $ TBN.EmptyPattern o      
        LB.VariablePattern o var -> TBN.VariablePattern o <$> toTBN var

-- | Convert a LittleBang binary operator to a TinyBang Nested binary operator      
instance TBNConvertible LB.BinaryOperator TBN.BinaryOperator where
  toTBN binaryOperator = return $ case binaryOperator of
        LB.OpIntPlus o -> TBN.OpIntPlus o
        LB.OpIntMinus o -> TBN.OpIntMinus o
        LB.OpIntEq o -> TBN.OpIntEq o 
        LB.OpIntGreaterEq o -> TBN.OpIntGreaterEq o
        LB.OpIntLessEq o -> TBN.OpIntLessEq o

-- | Convert a LittleBang primitive to a TinyBang Nested primitive         
instance TBNConvertible LB.PrimitiveType TBN.PrimitiveType where
  toTBN primitive = return $ case primitive of
        LB.PrimInt -> TBN.PrimInt

-- | Convert a LittleBang label to a TinyBang label      
instance TBNConvertible LB.LabelName TBN.LabelName where
  toTBN label = return $ case label of
        LB.LabelName o string -> TBN.LabelName o string

-- | Convert a LittleBang var to a TinyBang Nested var               
instance TBNConvertible LB.Var TBN.Var where
  toTBN var = return $ case var of
        LB.Var o string -> TBN.Var o string
