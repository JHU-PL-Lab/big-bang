module Language.LittleBang.Translator
( desugarLittleBang
, DesugarError(..)
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBang.Ast as TB
import Control.Applicative
import Control.Monad.State
import Data.Maybe

type DesugarM = StateT DesugarState (Either DesugarError)
data DesugarError
        = EmptyCaseClauseList LB.Expr
  deriving (Eq, Ord, Show)

data DesugarState = DesugarState { freshVarIdx :: Int }

type DesugarFunction a = a -> DesugarM a

class Desugarer t where
  exprDesugar :: t -> DesugarFunction LB.Expr
  patDesugar :: t -> DesugarFunction LB.Pattern
  outerPatDesugar :: t -> DesugarFunction LB.OuterPattern
  exprDesugar = const return
  patDesugar = const return
  outerPatDesugar = const return

-- | Desugar LittleBang. 
desugarLittleBang :: LB.Expr -> Either DesugarError LB.Expr
desugarLittleBang expr =
  runDesugarM $ foldl (>>=) (return expr) desugars
  where
    desugars =
        [ deepSubst DesugarIf
        , deepSubst DesugarCase
        , deepSubst DesugarList
        , deepSubst DesugarListPat
        ]

runDesugarM :: DesugarM a -> Either DesugarError a
runDesugarM x = fst <$> runStateT x (DesugarState 0)
    
substCaseClauses :: (Desugarer a) => a -> [LB.CaseClause] -> DesugarM [LB.CaseClause]    
substCaseClauses d = mapM substCaseClause
    where
        substCaseClause (LB.CaseClause o p e) = LB.CaseClause o <$> return p <*> deepSubst d e

substExprList :: (Desugarer a) => a -> [LB.Expr] -> [DesugarM LB.Expr]
substExprList d lst = case lst of
        hd : tl -> deepSubst d hd : substExprList d tl
        [] -> [] 

deepSubst :: (Desugarer a) => a -> LB.Expr -> DesugarM LB.Expr
deepSubst d e =
  let f = exprDesugar d in
  case e of
    LB.ExprList o lst1 -> do
       lst1' <- sequence (substExprList d lst1)
       let e' = LB.ExprList o lst1'
       f e'
    LB.ExprCase o e1 lst1 -> do
        e1' <- deepSubst d e1
        lst1' <- substCaseClauses d lst1
        let e' = LB.ExprCase o e1' lst1'
        f e' 
    LB.ExprCondition o e1 e2 e3 -> do
        e1' <- deepSubst d e1
        e2' <- deepSubst d e2
        e3' <- deepSubst d e3
        let e' = LB.ExprCondition o e1' e2' e3'
        f e'
    LB.ExprDef o var e1 e2 ->  do
        e1' <- deepSubst d e1
        e2' <- deepSubst d e2
        let e' = LB.ExprDef o var e1' e2'
        f e'
    LB.ExprVarIn o var e1 e2 -> do
        e1' <- deepSubst d e1
        e2' <- deepSubst d e2
        let e' = LB.ExprVarIn o var e1' e2'
        f e'
    LB.ExprScape o outerPattern e1 -> do
        e1' <- deepSubst d e1
        outerPattern' <- outerPatDesugar d outerPattern
        outerPattern'' <- case outerPattern' of
                            LB.OuterPatternLabel o' x p -> LB.OuterPatternLabel o' x <$> patDesugar d p
        let e' = LB.ExprScape o outerPattern'' e1'
        f e'               
    LB.ExprBinaryOp o e1 op e2 -> do
        e1' <- deepSubst d e1
        e2' <- deepSubst d e2 
        let e' = LB.ExprBinaryOp o e1' op e2'
        f e'           
    LB.ExprOnionOp o e1 onionOp projector -> do
        e1' <- deepSubst d e1
        let e' = LB.ExprOnionOp o e1' onionOp projector
        f e'
    LB.ExprOnion o e1 e2 -> do
        e1' <- deepSubst d e1
        e2' <- deepSubst d e2
        let e' = LB.ExprOnion o e1' e2'
        f e'
    LB.ExprAppl o e1 e2 -> do
        e1' <- deepSubst d e1
        e2' <- deepSubst d e2
        let e' = LB.ExprAppl o e1' e2'
        f e'
    LB.ExprLabelExp o label e1 -> do
        e1' <- deepSubst d e1
        let e' = LB.ExprLabelExp o label e1'
        f e'    
    LB.ExprVar o var -> f (LB.ExprVar o var)
    LB.ExprValInt o int -> f (LB.ExprValInt o int)
    LB.ExprValChar o char -> f (LB.ExprValChar o char)
    LB.ExprValUnit o -> f (LB.ExprValUnit o)   


-- | Desugar (if e1 then e2 else e3)
data DesugarIf = DesugarIf
instance Desugarer DesugarIf where
  exprDesugar DesugarIf expr = case expr of 
    LB.ExprCondition o e1 e2 e3 -> LB.ExprAppl o <$> 
                                   (LB.ExprOnion o 
                                        <$> (LB.ExprScape o 
                                                <$> (LB.OuterPatternLabel o 
                                                        <$> nextFreshVar 
                                                        <*> (LB.LabelPattern o 
                                                                <$> return (LB.LabelDef o "True") 
                                                                <*> nextFreshVar 
                                                                <*> return (LB.EmptyOnionPattern o)))        
                                                <*> return e2)
                                        <*> (LB.ExprScape o 
                                                <$> (LB.OuterPatternLabel o 
                                                        <$> nextFreshVar
                                                        <*> (LB.LabelPattern o 
                                                                <$> return (LB.LabelDef o "False") 
                                                                <*> nextFreshVar 
                                                                <*> return (LB.EmptyOnionPattern o))) 
                                                <*> return e3)
                                    )       
                                    <*> return e1
    _ -> return expr
   
-- | Desugar (case e of | p1 -> e1 | ... | pn -> en) 
data DesugarCase = DesugarCase
instance Desugarer DesugarCase where
  exprDesugar DesugarCase expr = case expr of 
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
-- | Desugar [e1,e2,...,en]                   
data DesugarList = DesugarList
instance Desugarer DesugarList where
  exprDesugar DesugarList expr = case expr of
    LB.ExprList o list -> toHeadTailList o list
    _ -> return expr
    where
    toHeadTailList :: TB.Origin -> [LB.Expr] -> DesugarM LB.Expr
    toHeadTailList o lst = case lst of
        [] -> return (LB.ExprLabelExp o (LB.LabelDef o "Nil") (LB.ExprValUnit o))                    
        e:tl -> LB.ExprOnion o 
                        <$> return (LB.ExprLabelExp o (LB.LabelDef o "Hd") e) 
                        <*> (LB.ExprLabelExp o <$> (LB.LabelDef o <$> return "Tl") <*> toHeadTailList o tl) 


-- | Desugar [p1,p2,...,pn]
--           [p1,p2,...,pn,`Nil junk:()...]    
data DesugarListPat = DesugarListPat
instance Desugarer DesugarListPat where
  patDesugar DesugarListPat pattern = case pattern of
    LB.ListPattern o list mEndPattern -> toHeadTailList o list (mEndPattern :: Maybe LB.Pattern)
    _ -> return pattern
    where
    toHeadTailList :: TB.Origin -> [LB.OuterPattern] -> Maybe LB.Pattern -> DesugarM LB.Pattern
    toHeadTailList o lst mEndPattern = case lst of
       [] ->
           maybe
             (LB.LabelPattern o (LB.LabelDef o "Nil") <$> nextFreshVar <*> return (LB.EmptyOnionPattern o))
             return
             mEndPattern             
       LB.OuterPatternLabel o2 v p:tl -> LB.ConjunctionPattern o 
                        <$> return (LB.LabelPattern o2 (LB.LabelDef o "Hd") v p) 
                        <*> (LB.LabelPattern o2 <$> (LB.LabelDef o <$> return "Tl") <*> nextFreshVar <*> toHeadTailList o tl mEndPattern) 
                                                       
-- | Get the next fresh variable.       
nextFreshVar :: DesugarM LB.Var
nextFreshVar = do
        s <- get
        let varName = "v" ++ show (freshVarIdx s)
        put $ s { freshVarIdx = freshVarIdx s + 1 }
        return $ LB.Var (TB.ComputedOrigin []) varName
