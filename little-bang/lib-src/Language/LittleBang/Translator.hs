module Language.LittleBang.Translator
( desugarLittleBang,
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBang.Ast as TB
import Control.Applicative
import Control.Monad.State

{-
  TODO: this system needs to be restructured.  In general, the translator should
  make use of a single deep substitution function.  Each of the desugarings
  should be written as shallow specializations.
-}

-- | Desugar LittleBang. Do nothing for now
desugarLittleBang :: LB.Expr -> Either DesugarError LB.Expr
desugarLittleBang expr =
  runDesugarM (return expr >>= walkExprTree)
  {-
  where
    desugars :: [LB.Expr -> DesugarM LB.Expr]
    desugars =
      [ desugarExprIf
      ]
  -}

runDesugarM :: DesugarM a -> Either DesugarError a
runDesugarM x = fst <$> runStateT x (DesugarState 0)

type DesugarM = StateT DesugarState (Either DesugarError)
type DesugarError = String

type DesugarFunction a = a -> DesugarM a

data DesugarState = DesugarState { freshVarIdx :: Int }

-- | Get an expr desugaring function based on an AST node
getExprDesugarer :: LB.Expr -> DesugarFunction LB.Expr
getExprDesugarer expr =
  case expr of
    LB.ExprCondition _ _ _ _ -> desugarExprIf
    LB.ExprSequence _ _ _ -> desugarExprSeq
    LB.ExprList _ _ -> desugarExprList
    LB.ExprRecord _ _ -> desugarExprRecord
    {-LB.ExprObject _ _ -> seal . desugarExprRecord . desugarExprObject-}
    _ -> desugarExprIdentity

-- | Get a pat desugaring function based on an AST node
getPatDesugarer :: LB.Pattern -> DesugarFunction LB.Pattern
getPatDesugarer pat = 
  case pat of
    LB.ListPattern _ _ _ -> desugarPatList
    _ -> desugarPatIdentity

getTermDesugarer :: LB.RecordTerm -> DesugarFunction LB.RecordTerm
getTermDesugarer tm = 
  case tm of
    LB.TermIdent _ _ _ -> desugarTermIdent
    LB.TermScape _ _ _ _ -> desugarTermScape
    LB.TermAnon _ _ _ -> desugarTermAnon
    LB.TermArgIdent _ _ -> desugarTermArgIdent
    LB.TermArgPat _ _ _ -> desugarTermArgPat

-- | Apply desugarers to the entire AST
-- Makes one pass, applying only appropriate desugarers
-- TODO: pull out the >>= f
walkExprTree :: LB.Expr -> DesugarM LB.Expr
walkExprTree expr = 
  let f = getExprDesugarer expr in
  case expr of 
    LB.ExprLet o var e1 e2 -> (LB.ExprLet o 
                                    <$> return var
                                    <*> walkExprTree e1
                                    <*> walkExprTree e2)
                                    >>= f
                                    
    LB.ExprScape o outerPattern e -> (LB.ExprScape o 
                                    <$> walkPatTree outerPattern 
                                    <*> walkExprTree e)
                                    >>= f
                                    
    LB.ExprBinaryOp o e1 op e2 -> (LB.ExprBinaryOp o 
                                    <$> walkExprTree e1 
                                    <*> return op
                                    <*> walkExprTree e2)
                                    >>= f
                                    
    LB.ExprOnion o e1 e2 -> (LB.ExprOnion o 
                                    <$> walkExprTree e1 
                                    <*> walkExprTree e2)
                                    >>= f
                                    
    LB.ExprAppl o e1 e2 -> (LB.ExprAppl o 
                                    <$> walkExprTree e1 
                                    <*> walkExprTree e2)
                                    >>= f
                                    
    LB.ExprLabelExp o label e1 -> (LB.ExprLabelExp o 
                                    <$> return label 
                                    <*> walkExprTree e1)
                                    >>= f
                                    
    LB.ExprRef o e -> (LB.ExprRef o <$> walkExprTree e) >>= f
    LB.ExprVar o var -> (LB.ExprVar o <$> return var) >>= f
    LB.ExprValInt o int -> (LB.ExprValInt o <$> return int) >>= f
    LB.ExprValEmptyOnion o -> (return $ LB.ExprValEmptyOnion o) >>= f

    LB.ExprCondition o e1 e2 e3 -> (LB.ExprCondition o
                                    <$> walkExprTree e1
                                    <*> walkExprTree e2
                                    <*> walkExprTree e3)
                                    >>= f
    LB.ExprSequence o e1 e2 -> (LB.ExprSequence o
                                    <$> walkExprTree e1
                                    <*> walkExprTree e2)
                                    >>= f
    LB.ExprList o e -> (LB.ExprList o
                                    <$> mapM walkExprTree e)
                                    >>= f
    LB.ExprRecord o e -> (LB.ExprRecord o
                                    <$> mapM walkTermTree e)
                                    >>= f
    LB.ExprObject o e -> (LB.ExprObject o
                                    <$> walkExprTree e)
                                    >>= f

-- |Walk the tree of patterns, applying desugarers
walkPatTree :: LB.Pattern -> DesugarM LB.Pattern
walkPatTree pat = 
  let f = getPatDesugarer pat in
  case pat of
    LB.PrimitivePattern o prim -> (LB.PrimitivePattern o
                                    <$> return prim)
                                    >>= f
    LB.LabelPattern o label p -> (LB.LabelPattern o
                                    <$> return label
                                    <*> walkPatTree p)
                                    >>= f
    LB.RefPattern o p -> (LB.RefPattern o
                            <$> walkPatTree p)
                            >>= f
    LB.ConjunctionPattern o p1 p2 -> (LB.ConjunctionPattern o
                                    <$> walkPatTree p1
                                    <*> walkPatTree p2)
                                    >>= f
    LB.EmptyPattern o -> (return $ LB.EmptyPattern o) >>= f
    LB.VariablePattern o var -> (LB.VariablePattern o <$> return var) >>= f
    LB.ListPattern o p t -> (LB.ListPattern o
                                    <$> mapM walkPatTree p)
                                    <*> (case t of
                                            Just p' -> Just <$> walkPatTree p'
                                            Nothing -> return Nothing
                                        )
                                    >>= f

-- |Walk the tree of record terms, a
walkTermTree :: LB.RecordTerm -> DesugarM LB.RecordTerm
walkTermTree tm = 
  let f = getTermDesugarer tm in
  case tm of
    LB.TermIdent o l e -> (LB.TermIdent o
                                    <$> return l
                                    <*> walkExprTree e)
                                    >>= f
    LB.TermScape o l ts e -> (LB.TermScape o
                                    <$> return l
                                    <*> mapM walkTermTree ts
                                    <*> walkExprTree e)
                                    >>= f
    LB.TermAnon o ts e -> (LB.TermAnon o
                                    <$> mapM walkTermTree ts
                                    <*> walkExprTree e)
                                    >>= f
    LB.TermArgIdent o l -> (LB.TermArgIdent o <$> return l) >>= f
    LB.TermArgPat o l p -> (LB.TermArgPat o
                                    <$> return l
                                    <*> walkPatTree p)
                                    >>= f
    _ -> return tm

-- |Desugar (if e1 then e2 else e3)
-- For en' := desugar en
-- The expression becomes
-- ((`True () -> e2') & (`False () -> e3')) e1';;
desugarExprIf :: LB.Expr -> DesugarM LB.Expr
desugarExprIf expr =
  case expr of
    LB.ExprCondition o e1 e2 e3 ->
      LB.ExprAppl o <$>
        (LB.ExprOnion o <$>
          (LB.ExprScape o 
            (LB.LabelPattern o (LB.LabelName o "True") (LB.EmptyPattern o))
            <$> return e2) <*>
          (LB.ExprScape o 
            (LB.LabelPattern o (LB.LabelName o "False") (LB.EmptyPattern o))
            <$> return e3)) <*>
        (return e1)    
    _ -> return expr

desugarExprSeq :: LB.Expr -> DesugarM LB.Expr
desugarExprSeq expr =
  case expr of
    LB.ExprSequence o e1 e2 -> 
      LB.ExprAppl o <$>
        (LB.ExprScape o
          (LB.LabelPattern o (LB.LabelName o "Seq") (LB.EmptyPattern o))
          <$> return e2) <*>
        (LB.ExprLabelExp o <$>
          return (LB.LabelName o "Seq") <*>
          return e1)
    _ -> return expr

desugarExprList :: LB.Expr -> DesugarM LB.Expr
desugarExprList expr = 
  case expr of
    LB.ExprList o list -> toHTList o list
    _ -> return expr
    where
    toHTList :: TB.Origin -> [LB.Expr] -> DesugarM LB.Expr
    toHTList o lst = case lst of
      [] -> return (LB.ExprLabelExp o (LB.LabelName o "Nil") (LB.ExprValEmptyOnion o))
      e:t -> LB.ExprOnion o
                <$> return (LB.ExprLabelExp o (LB.LabelName o "Hd") e)
                <*> (LB.ExprLabelExp o <$> (LB.LabelName o <$> return "Tl") <*> toHTList o t)

desugarExprRecord :: LB.Expr -> DesugarM LB.Expr
desugarExprRecord expr =
  case expr of
    LB.ExprRecord o list ->
        let val = foldl (\a b -> LB.ExprOnion o b a) (unTermExpr $ head list) (map unTermExpr $ tail list) in return val
    _ -> return expr

-- |An identity desugarer which does nothing
-- This is applied to all elements of LB that are also in TBN
desugarExprIdentity :: LB.Expr -> DesugarM LB.Expr
desugarExprIdentity expr = 
  case expr of
    LB.ExprLet o var e1 e2 -> LB.ExprLet o 
                                    <$> return var 
                                    <*> return e1 
                                    <*> return e2
                                    
    LB.ExprScape o outerPattern e -> LB.ExprScape o 
                                    <$> return outerPattern 
                                    <*> return e
                                    
    LB.ExprBinaryOp o e1 op e2 -> LB.ExprBinaryOp o 
                                    <$> return e1 
                                    <*> return op
                                    <*> return e2
                                    
    LB.ExprOnion o e1 e2 -> LB.ExprOnion o 
                                    <$> return e1 
                                    <*> return e2
                                    
    LB.ExprAppl o e1 e2 -> LB.ExprAppl o 
                                    <$> return e1 
                                    <*> return e2
                                    
    LB.ExprLabelExp o label e1 -> LB.ExprLabelExp o 
                                    <$> return label 
                                    <*> return e1
                                    
    LB.ExprRef o e -> LB.ExprRef o <$> return e
    LB.ExprVar o var -> LB.ExprVar o <$> return var
    LB.ExprValInt o int -> LB.ExprValInt o <$> return int
    LB.ExprValEmptyOnion o -> return (LB.ExprValEmptyOnion o)
    _ -> return expr

desugarPatList :: LB.Pattern -> DesugarM LB.Pattern
desugarPatList pat = 
  case pat of
    LB.ListPattern o list end -> toHTList o list end
    _ -> return pat
    where
    toHTList :: TB.Origin -> [LB.Pattern] -> (Maybe LB.Pattern) -> DesugarM LB.Pattern
    toHTList o lst end = case lst of
      [] -> getListContinuation o end
      p:t -> LB.ConjunctionPattern o
                <$> return (LB.LabelPattern o (LB.LabelName o "Hd") p)
                <*> (LB.LabelPattern o <$> (LB.LabelName o <$> return "Tl") <*> toHTList o t end)
    getListContinuation :: TB.Origin -> (Maybe LB.Pattern) -> DesugarM LB.Pattern
    getListContinuation o end = case end of
      Nothing -> return (LB.LabelPattern o (LB.LabelName o "Nil") (LB.EmptyPattern o))
      Just p -> return p
      

desugarPatIdentity :: LB.Pattern -> DesugarM LB.Pattern
desugarPatIdentity pat = 
  case pat of
    LB.PrimitivePattern o prim -> LB.PrimitivePattern o <$> return prim
    LB.LabelPattern o label p -> LB.LabelPattern o
                                    <$> return label
                                    <*> return p
    LB.ConjunctionPattern o p1 p2 -> LB.ConjunctionPattern o
                                    <$> return p1
                                    <*> return p2
    LB.EmptyPattern o -> (return $ LB.EmptyPattern o)
    LB.VariablePattern o v -> LB.VariablePattern o <$> return v
    _ -> return pat

-- |desugarers for RecordTerm
desugarTermIdent :: LB.RecordTerm -> DesugarM LB.RecordTerm
desugarTermIdent tm = 
  case tm of
    LB.TermIdent o label expr -> LB.TermNativeExpr o <$> (LB.ExprLabelExp o <$> return label <*> return expr)
    _ -> return tm

desugarTermScape :: LB.RecordTerm -> DesugarM LB.RecordTerm
desugarTermScape tm = 
  case tm of
    LB.TermScape o label args body -> 
        let identPart = LB.LabelPattern o (LB.LabelName o "_msg") (LB.LabelPattern o label (LB.EmptyPattern o)) in
        let argPart = foldl (\a b -> LB.ConjunctionPattern o b a) (unTermPat $ head args) (map unTermPat $ tail args) in
        let pat = LB.ConjunctionPattern o identPart argPart in
        return $ LB.TermNativeExpr o (LB.ExprScape o pat body)
    _ -> return tm

desugarTermAnon :: LB.RecordTerm -> DesugarM LB.RecordTerm
desugarTermAnon tm = 
  case tm of
    LB.TermAnon _ _ _ -> return tm -- TODO
    _ -> return tm

desugarTermArgIdent :: LB.RecordTerm -> DesugarM LB.RecordTerm
desugarTermArgIdent tm = 
  case tm of
    LB.TermArgIdent o l -> LB.TermNativePat o <$> (LB.LabelPattern o <$>
                                                return l
                                                <*> (LB.VariablePattern o
                                                        <$> (LB.Var o
                                                            <$> (return $ LB.unLabelName l)
                                                        )
                                                    )
                                                )
    _ -> return tm

desugarTermArgPat :: LB.RecordTerm -> DesugarM LB.RecordTerm
desugarTermArgPat tm = 
  case tm of
    LB.TermArgPat _ _ _ -> return tm -- TODO
    _ -> return tm

desugarTermIdentity :: LB.RecordTerm -> DesugarM LB.RecordTerm
desugarTermIdentity tm = return tm

unTermExpr :: LB.RecordTerm -> LB.Expr
unTermExpr (LB.TermNativeExpr _ e) = e
unTermPat  (LB.TermNativePat  _ p) = p

-- seal function
{-
seal :: LB.Expr -> LB.Expr
seal e = 
 -}   
          
nextFreshVar :: DesugarM LB.Var
nextFreshVar = do
  s <- get
  let varName = "v" ++ show (freshVarIdx s)
  put $ s { freshVarIdx = freshVarIdx s + 1 }
  return $ LB.Var (TB.ComputedOrigin []) varName
