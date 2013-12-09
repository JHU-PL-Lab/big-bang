module Language.LittleBang.Translator
( desugarLittleBang,
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBang.Ast as TB
import qualified Language.TinyBangNested.Ast as TBN
import Control.Applicative
import Control.Monad.State

-- | Desugar LittleBang. Do nothing for now
desugarLittleBang :: LB.Expr -> Either DesugarError LB.Expr
desugarLittleBang expr =
  runDesugarM $ foldl (>>=) (return expr) desugars
  where
    desugars :: [LB.Expr -> DesugarM LB.Expr]
    desugars =
      [ desugarIf
      ]

runDesugarM :: DesugarM a -> Either DesugarError a
runDesugarM x = fst <$> runStateT x (DesugarState 0)

type DesugarM = StateT DesugarState (Either DesugarError)
type DesugarError = String

data DesugarState = DesugarState { freshVarIdx :: Int }

-- Desugar (if e1 then e2 else e3)
-- For en' := desugar en
-- The expression becomes
-- ((x1: `True x2:() -> e2') & (x3: `False x4:() -> e3')) e1';;
desugarIf :: LB.Expr -> DesugarM LB.Expr
desugarIf expr = case expr of 

    LB.ExprCondition o e1 e2 e3 -> LB.ExprAppl o <$> 
                                   (LB.ExprOnion o 
                                        <$> (LB.ExprScape o 
                                                <$> (LB.OuterPatternLabel o 
                                                        <$> nextFreshVar 
                                                        <*> (LB.LabelPattern o 
                                                                <$> return (LB.LabelDef o "True") 
                                                                <*> nextFreshVar 
                                                                <*> return (LB.EmptyOnionPattern o)))        
                                                <*> desugarIf e2)
                                        <*> (LB.ExprScape o 
                                                <$> (LB.OuterPatternLabel o 
                                                        <$> nextFreshVar
                                                        <*> (LB.LabelPattern o 
                                                                <$> return (LB.LabelDef o "False") 
                                                                <*> nextFreshVar 
                                                                <*> return (LB.EmptyOnionPattern o))) 
                                                <*> desugarIf e3)
                                    )       
                                    <*> desugarIf e1
    
    LB.ExprDef o var e1 e2 -> LB.ExprDef o 
                                    <$> return var 
                                    <*> desugarIf e1 
                                    <*> desugarIf e2
                                    
    LB.ExprVarIn o var e1 e2 -> LB.ExprVarIn o 
                                    <$> return var 
                                    <*> desugarIf e1
                                    <*> desugarIf e2
                                    
    LB.ExprScape o outerPattern e -> LB.ExprScape o 
                                    <$> return outerPattern 
                                    <*> desugarIf e
                                    
    LB.ExprBinaryOp o e1 op e2 -> LB.ExprBinaryOp o 
                                    <$> desugarIf e1 
                                    <*> return op
                                    <*> desugarIf e2
                                    
    LB.ExprOnionOp o e onionOp projector -> LB.ExprOnionOp o 
                                    <$> desugarIf e 
                                    <*> return onionOp
                                    <*> return projector
                                    
    LB.ExprOnion o e1 e2 -> LB.ExprOnion o 
                                    <$> desugarIf e1 
                                    <*> desugarIf e2
                                    
    LB.ExprAppl o e1 e2 -> LB.ExprAppl o 
                                    <$> desugarIf e1 
                                    <*> desugarIf e2
                                    
    LB.ExprLabelExp o label e1 -> LB.ExprLabelExp o 
                                    <$> return label 
                                    <*> desugarIf e1
                                    
    LB.ExprVar o var -> LB.ExprVar o <$> return var
    LB.ExprValInt o int -> LB.ExprValInt o <$> return int
    LB.ExprValChar o char -> LB.ExprValChar o <$> return char
    LB.ExprValUnit o -> return $ LB.ExprValUnit o
          
nextFreshVar :: DesugarM LB.Var
nextFreshVar = do
  s <- get
  let varName = "v" ++ show (freshVarIdx s)
  put $ s { freshVarIdx = freshVarIdx s + 1 }
  return $ LB.Var (TB.ComputedOrigin []) varName
