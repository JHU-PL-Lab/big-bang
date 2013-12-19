module Language.LittleBang.Translator
( desugarLittleBang
, DesugarError(..)
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBang.Ast as TB
import Control.Applicative
import Control.Monad.State

type DesugarM = StateT DesugarState (Either DesugarError)
data DesugarError
        = EmptyCaseClauseList LB.Expr
  deriving (Eq, Ord, Show)

data DesugarState = DesugarState { freshVarIdx :: Int }

-- | Desugar LittleBang. 
desugarLittleBang :: LB.Expr -> Either DesugarError LB.Expr
desugarLittleBang expr =
  runDesugarM $ foldl (>>=) (return expr) desugars
  where
    desugars :: [LB.Expr -> DesugarM LB.Expr]
    desugars = [ deepSubst desugarIf, deepSubst desugarCase, deepSubst desugarList ]

runDesugarM :: DesugarM a -> Either DesugarError a
runDesugarM x = fst <$> runStateT x (DesugarState 0)
    
substCaseClauses :: (LB.Expr -> DesugarM LB.Expr) -> [LB.CaseClause] -> DesugarM [LB.CaseClause]    
substCaseClauses f = mapM substCaseClause
    where
        substCaseClause (LB.CaseClause o p e) = LB.CaseClause o <$> return p <*> deepSubst f e

substExprList :: (LB.Expr -> DesugarM LB.Expr) -> [LB.Expr] -> [DesugarM LB.Expr]
substExprList f lst = case lst of
        hd : tl -> deepSubst f hd : substExprList f tl
        [] -> [] 

deepSubst :: (LB.Expr -> DesugarM LB.Expr) -> LB.Expr -> DesugarM LB.Expr
deepSubst f e = case e of
    LB.ExprList o lst1 -> do
       lst1' <- sequence (substExprList f lst1)
       let e' = LB.ExprList o lst1'
       f e'
    LB.ExprCase o e1 lst1 -> do
        e1' <- deepSubst f e1
        lst1' <- substCaseClauses f lst1
        let e' = LB.ExprCase o e1' lst1'
        f e' 
    LB.ExprCondition o e1 e2 e3 -> do
        e1' <- deepSubst f e1
        e2' <- deepSubst f e2
        e3' <- deepSubst f e3
        let e' = LB.ExprCondition o e1' e2' e3'
        f e'
    LB.ExprDef o var e1 e2 ->  do
        e1' <- deepSubst f e1
        e2' <- deepSubst f e2
        let e' = LB.ExprDef o var e1' e2'
        f e'
    LB.ExprVarIn o var e1 e2 -> do
        e1' <- deepSubst f e1
        e2' <- deepSubst f e2
        let e' = LB.ExprVarIn o var e1' e2'
        f e'
    LB.ExprScape o outerPattern e1 -> do
        e1' <- deepSubst f e1
        let e' = LB.ExprScape o outerPattern e1'
        f e'               
    LB.ExprBinaryOp o e1 op e2 -> do
        e1' <- deepSubst f e1
        e2' <- deepSubst f e2 
        let e' = LB.ExprBinaryOp o e1' op e2'
        f e'           
    LB.ExprOnionOp o e1 onionOp projector -> do
        e1' <- deepSubst f e1
        let e' = LB.ExprOnionOp o e1' onionOp projector
        f e'
    LB.ExprOnion o e1 e2 -> do
        e1' <- deepSubst f e1
        e2' <- deepSubst f e2
        let e' = LB.ExprOnion o e1' e2'
        f e'
    LB.ExprAppl o e1 e2 -> do
        e1' <- deepSubst f e1
        e2' <- deepSubst f e2
        let e' = LB.ExprAppl o e1' e2'
        f e'
    LB.ExprLabelExp o label e1 -> do
        e1' <- deepSubst f e1
        let e' = LB.ExprLabelExp o label e1'
        f e'    
    LB.ExprVar o var -> f (LB.ExprVar o var)
    LB.ExprValInt o int -> f (LB.ExprValInt o int)
    LB.ExprValChar o char -> f (LB.ExprValChar o char)
    LB.ExprValUnit o -> f (LB.ExprValUnit o)   

-- Desugar (if e1 then e2 else e3)
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
    _ -> return expr
   
-- Desugar (case e of | p1 -> e1 | ... | pn -> en) 
desugarCase :: LB.Expr -> DesugarM LB.Expr   
desugarCase expr = case expr of 
    LB.ExprCase o e1 list -> LB.ExprAppl o <$> caseClauseListToOnions (reverse list) <*> return e1
    _ -> return expr
    where
    caseClauseListToOnions :: [LB.CaseClause] -> DesugarM LB.Expr
    caseClauseListToOnions lst = case lst of
        [] -> lift $ Left $ EmptyCaseClauseList expr
        c : [] -> clauseToScape c
        c : tl -> LB.ExprOnion (TB.originOf c) 
                                        <$> clauseToScape c
                                        <*> caseClauseListToOnions tl
        where
          clauseToScape :: LB.CaseClause -> DesugarM LB.Expr
          clauseToScape (LB.CaseClause o p e) = LB.ExprScape o 
                                                        <$> (LB.OuterPatternLabel o 
                                                                <$> nextFreshVar 
                                                                <*> return p) 
                                                        <*> return e
                   
desugarList :: LB.Expr -> DesugarM LB.Expr
desugarList expr = case expr of
    LB.ExprList o list -> toHeadTailList o list
    _ -> return expr
    where
    toHeadTailList :: TB.Origin -> [LB.Expr] -> DesugarM LB.Expr
    toHeadTailList o lst = case lst of
        [] -> return (LB.ExprLabelExp o (LB.LabelDef o "Tl") (LB.ExprLabelExp o (LB.LabelDef o "Nil") (LB.ExprValUnit o)))                    
        e:tl -> LB.ExprOnion o 
                        <$> return (LB.ExprLabelExp o (LB.LabelDef o "Hd") e) 
                        <*> (LB.ExprLabelExp o <$> (LB.LabelDef o <$> return "Tl") <*> toHeadTailList o tl) 
                   
-- Get the next fresh variable.       
nextFreshVar :: DesugarM LB.Var
nextFreshVar = do
        s <- get
        let varName = "v" ++ show (freshVarIdx s)
        put $ s { freshVarIdx = freshVarIdx s + 1 }
        return $ LB.Var (TB.ComputedOrigin []) varName
