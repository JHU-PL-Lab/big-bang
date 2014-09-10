{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Language.LittleBang.TBNConversion
( convertToTBNExpr
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBangNested.Ast as TBN
import Control.Applicative
import Language.TinyBang.Utils.Display

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
    LB.TExprLet o var e1 e2 -> TBN.ExprLet o 
                                    <$> toTBN var 
                                    <*> toTBN e1 
                                    <*> toTBN e2
                                    
    LB.TExprScape o outerPattern e -> TBN.ExprScape o 
                                    <$> toTBN outerPattern 
                                    <*> toTBN e
                                   
    LB.TExprBinaryOp o e1 op e2 -> TBN.ExprBinaryOp o 
                                    <$> toTBN e1 
                                    <*> toTBN op
                                    <*> toTBN e2
                                    
    LB.TExprOnion o e1 e2 -> TBN.ExprOnion o 
                                    <$> toTBN e1 
                                    <*> toTBN e2
                                    
    LB.TExprAppl o e1 e2 -> TBN.ExprAppl o 
                                    <$> toTBN e1 
                                    <*> toTBN e2
                                    
    LB.TExprLabelExp o label e1 -> TBN.ExprLabelExp o 
                                    <$> toTBN label 
                                    <*> toTBN e1

    LB.TExprRef o e1 -> TBN.ExprRef o <$> toTBN e1
                                    
    LB.TExprVar o var -> TBN.ExprVar o <$> toTBN var
    LB.TExprValInt o i -> return $ TBN.ExprValInt o i
    LB.TExprValChar o i -> return $ TBN.ExprValChar o i
    LB.TExprValEmptyOnion o -> return $ TBN.ExprValEmptyOnion o
    LB.TExprGetChar o -> return $ TBN.ExprGetChar o
    LB.TExprPutChar o e -> TBN.ExprPutChar o <$> toTBN e
    _ -> error $ "Cannot TBN convert: " ++ display expr -- TODO: get a correct failure mode
        
-- | Convert a LittleBang pattern to a TinyBang Nested pattern         
instance TBNConvertible LB.Pattern TBN.Pattern where
  toTBN pattern = case pattern of   
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
    _ -> error $ "Cannot TBN convert: " ++ display pattern -- TODO: get a correct failure mode

-- | Convert a LittleBang binary operator to a TinyBang Nested binary operator      

instance TBNConvertible TBN.BinaryOperator TBN.BinaryOperator where
  toTBN binaryOperator = return $ case binaryOperator of
    _ -> binaryOperator -- TODO: try to get rid of this function call
{-
        LB.OpIntPlus o -> TBN.OpIntPlus o
        LB.OpIntMinus o -> TBN.OpIntMinus o
        LB.OpIntEq o -> TBN.OpIntEq o 
        LB.OpIntGreaterEq o -> TBN.OpIntGreaterEq o
        LB.OpIntLessEq o -> TBN.OpIntLessEq o
        LB.OpSet o -> TBN.OpSet o
-}

-- | Convert a LittleBang primitive to a TinyBang Nested primitive         
instance TBNConvertible LB.PrimitiveType TBN.PrimitiveType where
  toTBN primitive = return $ case primitive of
        LB.PrimInt -> TBN.PrimInt
        LB.PrimChar -> TBN.PrimChar

-- | Convert a LittleBang label to a TinyBang label      
instance TBNConvertible LB.LabelName TBN.LabelName where
  toTBN label = return $ case label of
        LB.LabelName o str -> TBN.LabelName o str

-- | Convert a LittleBang var to a TinyBang Nested var               
instance TBNConvertible LB.Ident TBN.Ident where
  toTBN var = return $ case var of
        LB.Ident o str -> TBN.Ident o str
