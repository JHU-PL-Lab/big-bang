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
    _ -> desugarExprIdentity

-- | Get a pat desugaring function based on an AST node
getPatDesugarer :: LB.Pattern -> DesugarFunction LB.Pattern
getPatDesugarer pat = 
  case pat of
    LB.ListPattern _ _ _ -> desugarPatList
    _ -> desugarPatIdentity

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
          
nextFreshVar :: DesugarM LB.Var
nextFreshVar = do
  s <- get
  let varName = "v" ++ show (freshVarIdx s)
  put $ s { freshVarIdx = freshVarIdx s + 1 }
  return $ LB.Var (TB.ComputedOrigin []) varName
