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
    
    LB.ExprDef o var e1 e2 -> TBN.ExprDef o 
                                    <$> toTBN var 
                                    <*> toTBN e1 
                                    <*> toTBN e2
                                    
    LB.ExprVarIn o var e1 e2 -> TBN.ExprVarIn o 
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
                                    
    LB.ExprOnionOp o e onionOp projector -> TBN.ExprOnionOp o 
                                    <$> toTBN e 
                                    <*> toTBN onionOp
                                    <*> toTBN projector
                                    
    LB.ExprOnion o e1 e2 -> TBN.ExprOnion o 
                                    <$> toTBN e1 
                                    <*> toTBN e2
                                    
    LB.ExprAppl o e1 e2 -> TBN.ExprAppl o 
                                    <$> toTBN e1 
                                    <*> toTBN e2
                                    
    LB.ExprLabelExp o label e1 -> TBN.ExprLabelExp o 
                                    <$> toTBN label 
                                    <*> toTBN e1
                                    
    LB.ExprVar o var -> TBN.ExprVar o <$> toTBN var
    LB.ExprValInt o int -> return $ TBN.ExprValInt o int
    LB.ExprValChar o char -> return $ TBN.ExprValChar o char
    LB.ExprValUnit o -> return $ TBN.ExprValUnit o
        
-- | Convert a LittleBang outer pattern to a TinyBang Nested outer pattern       
instance TBNConvertible LB.OuterPattern TBN.OuterPattern where
  toTBN outerPattern = case outerPattern of
    LB.OuterPatternLabel o var pattern ->
      TBN.OuterPatternLabel o <$> toTBN var <*> toTBN pattern  

-- | Convert a LittleBang pattern to a TinyBang Nested pattern         
instance TBNConvertible LB.Pattern TBN.Pattern where
  toTBN pattern = case pattern of   
        LB.PrimitivePattern o primitive ->
          TBN.PrimitivePattern o <$> toTBN primitive
        LB.LabelPattern o label var p ->
          TBN.LabelPattern o <$> toTBN label <*> toTBN var <*> toTBN p
        LB.ConjunctionPattern o p1 p2 ->
          TBN.ConjunctionPattern o <$> toTBN p1 <*> toTBN p2
        LB.ScapePattern o -> return $ TBN.ScapePattern o
        LB.EmptyOnionPattern o -> return $ TBN.EmptyOnionPattern o      

-- | Convert a LittleBang projector to a TinyBang Nested projector       
instance TBNConvertible LB.Projector TBN.Projector where
  toTBN projector = case projector of
        LB.PrimitiveProjector o primitive ->
          TBN.PrimitiveProjector o <$> toTBN primitive
        LB.LabelProjector o label ->
          TBN.LabelProjector o <$> toTBN label
        LB.FunProjector o -> return $ TBN.FunProjector o 

-- | Convert a LittleBang onion operator to a TinyBang Nested onion operator        
instance TBNConvertible LB.OnionOperator TBN.OnionOperator where
  toTBN onionOperator = return $ case onionOperator of
        LB.OpOnionSub o -> TBN.OpOnionSub o       
        LB.OpOnionProj o -> TBN.OpOnionProj o 

-- | Convert a LittleBang binary operator to a TinyBang Nested binary operator      
instance TBNConvertible LB.BinaryOperator TBN.BinaryOperator where
  toTBN binaryOperator = return $ case binaryOperator of
        LB.OpPlus o -> TBN.OpPlus o
        LB.OpMinus o -> TBN.OpMinus o
        LB.OpEqual o -> TBN.OpEqual o 
        LB.OpGreater o -> TBN.OpGreater o 
        LB.OpGreaterEq o -> TBN.OpGreaterEq o
        LB.OpLesser o -> TBN.OpLesser o 
        LB.OpLesserEq o -> TBN.OpLesserEq o

-- | Convert a LittleBang primitive to a TinyBang Nested primitive         
instance TBNConvertible LB.Primitive TBN.Primitive where
  toTBN primitive = return $ case primitive of
        LB.TInt o -> TBN.TInt o 
        LB.TChar o -> TBN.TChar o 

-- | Convert a LittleBang label to a TinyBang label      
instance TBNConvertible LB.Label TBN.Label where
  toTBN label = return $ case label of
        LB.LabelDef o string -> TBN.LabelDef o string

-- | Convert a LittleBang var to a TinyBang Nested var               
instance TBNConvertible LB.Var TBN.Var where
  toTBN var = return $ case var of
        LB.Var o string -> TBN.Var o string
