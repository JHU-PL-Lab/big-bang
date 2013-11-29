module Language.LittleBang.Translator

( 
desugarLittleBang,
convertToTBNExpr
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBangNested.Ast as TBN
import Control.Applicative   

-- | Desugar LittleBang. Do nothing for now
desugarLittleBang :: LB.Expr -> Either String LB.Expr
desugarLittleBang expr = Right expr

-- | Convert a LittleBang expression to a TinyBang Nested expression 
convertToTBNExpr :: LB.Expr -> Either String TBN.Expr
convertToTBNExpr expr = toTBNExpr expr

-- | Convert a LittleBang expression to a TinyBang Nested expression 
toTBNExpr :: LB.Expr -> Either String TBN.Expr
toTBNExpr expr = case expr of 

        LB.ExprCondition _ _ _ _ -> Left "No translation for 'if e1 then e2 else e3' yet"
        
        LB.ExprDef o var e1 e2 -> TBN.ExprDef o 
                                        <$> return (toTBNVar var) 
                                        <*> toTBNExpr e1 
                                        <*> toTBNExpr e2
                                        
        LB.ExprVarIn o var e1 e2 -> TBN.ExprVarIn o 
                                        <$> return (toTBNVar var) 
                                        <*> toTBNExpr e1
                                        <*> toTBNExpr e2
                                        
        LB.ExprScape o outerPattern e -> TBN.ExprScape o 
                                        <$> return (toTBNOuterPattern outerPattern) 
                                        <*> toTBNExpr e
                                        
        LB.ExprBinaryOp o e1 op e2 -> TBN.ExprBinaryOp o 
                                        <$> toTBNExpr e1 
                                        <*> return (toTBNBinaryOp op) 
                                        <*> toTBNExpr e2
                                        
        LB.ExprOnionOp o e onionOp projector -> TBN.ExprOnionOp o 
                                        <$> toTBNExpr e 
                                        <*> return (toTBNOnionOp onionOp) 
                                        <*> return (toTBNProjector projector)
                                        
        LB.ExprOnion o e1 e2 -> TBN.ExprOnion o 
                                        <$> toTBNExpr e1 
                                        <*> toTBNExpr e2
                                        
        LB.ExprAppl o e1 e2 -> TBN.ExprAppl o 
                                        <$> toTBNExpr e1 
                                        <*> toTBNExpr e2
                                        
        LB.ExprLabelExp o label e1 -> TBN.ExprLabelExp o 
                                        <$> return (toTBNLabel label) 
                                        <*> toTBNExpr e1
                                        
        LB.ExprVar o var -> return $ TBN.ExprVar o $ toTBNVar var
        LB.ExprValInt o int -> return $ TBN.ExprValInt o int
        LB.ExprValChar o char -> return $ TBN.ExprValChar o char
        LB.ExprValUnit o -> return $ TBN.ExprValUnit o
        
-- | Convert a LittleBang outer pattern to a TinyBang Nested outer pattern       
toTBNOuterPattern :: LB.OuterPattern -> TBN.OuterPattern
toTBNOuterPattern outerPattern = case outerPattern of
        LB.OuterPatternLabel o var pattern -> TBN.OuterPatternLabel o 
                                                (toTBNVar var) 
                                                (toTBNPattern pattern)  

-- | Convert a LittleBang pattern to a TinyBang Nested pattern         
toTBNPattern :: LB.Pattern -> TBN.Pattern
toTBNPattern pattern = case pattern of   
        LB.PrimitivePattern o primitive -> TBN.PrimitivePattern o 
                                                (toTBNPrimitive primitive)
        LB.LabelPattern o label var p -> TBN.LabelPattern o 
                                                (toTBNLabel label) 
                                                (toTBNVar var)
                                                (toTBNPattern p)
        LB.ConjunctionPattern o p1 p2 -> TBN.ConjunctionPattern o 
                                                (toTBNPattern p1) 
                                                (toTBNPattern p2)
        LB.ScapePattern o -> TBN.ScapePattern o
        LB.EmptyOnionPattern o -> TBN.EmptyOnionPattern o      

-- | Convert a LittleBang projector to a TinyBang Nested projector       
toTBNProjector :: LB.Projector -> TBN.Projector
toTBNProjector projector = case projector of
        LB.PrimitiveProjector o primitive -> TBN.PrimitiveProjector o 
                                                (toTBNPrimitive primitive)
        LB.LabelProjector o label -> TBN.LabelProjector o 
                                                (toTBNLabel label)
        LB.FunProjector o -> TBN.FunProjector o 

-- | Convert a LittleBang onion operator to a TinyBang Nested onion operator        
toTBNOnionOp :: LB.OnionOperator -> TBN.OnionOperator
toTBNOnionOp onionOperator = case onionOperator of
        LB.OpOnionSub o -> TBN.OpOnionSub o       
        LB.OpOnionProj o -> TBN.OpOnionProj o 

-- | Convert a LittleBang binary operator to a TinyBang Nested binary operator      
toTBNBinaryOp :: LB.BinaryOperator -> TBN.BinaryOperator
toTBNBinaryOp binaryOperator = case binaryOperator of
        LB.OpPlus o -> TBN.OpPlus o
        LB.OpMinus o -> TBN.OpMinus o
        LB.OpEqual o -> TBN.OpEqual o 
        LB.OpGreater o -> TBN.OpGreater o 
        LB.OpGreaterEq o -> TBN.OpGreaterEq o
        LB.OpLesser o -> TBN.OpLesser o 
        LB.OpLesserEq o -> TBN.OpLesserEq o

-- | Convert a LittleBang primitive to a TinyBang Nested primitive         
toTBNPrimitive :: LB.Primitive -> TBN.Primitive
toTBNPrimitive primitive = case primitive of
        LB.TInt o -> TBN.TInt o 
        LB.TChar o -> TBN.TChar o 

-- | Convert a LittleBang label to a TinyBang label      
toTBNLabel :: LB.Label -> TBN.Label
toTBNLabel label = case label of
        LB.LabelDef o string -> TBN.LabelDef o string

-- | Convert a LittleBang var to a TinyBang Nested var               
toTBNVar :: LB.Var -> TBN.Var
toTBNVar var = case var of
        LB.Var o string -> TBN.Var o string 