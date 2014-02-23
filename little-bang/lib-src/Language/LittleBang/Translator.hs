module Language.LittleBang.Translator
( desugarLittleBang,
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBang.Ast as TB
import qualified Language.TinyBangNested.Ast as TBN
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
  runDesugarM ((return expr) >>= walkTree)
  {-
  where
    desugars :: [LB.Expr -> DesugarM LB.Expr]
    desugars =
      [ desugarIf
      ]
  -}

runDesugarM :: DesugarM a -> Either DesugarError a
runDesugarM x = fst <$> runStateT x (DesugarState 0)

type DesugarM = StateT DesugarState (Either DesugarError)
type DesugarError = String

data DesugarState = DesugarState { freshVarIdx :: Int }

-- Get a desugaring function based on an AST node
getDesugarer :: LB.Expr -> (LB.Expr -> DesugarM LB.Expr)
getDesugarer expr =
  case expr of
    LB.ExprCondition _ _ _ _ -> desugarIf
    LB.ExprSequence _ _ _ -> desugarSeq
    LB.ExprList _ _ -> desugarList
    _ -> desugarIdentity

-- Apply desugarers to the entire AST
-- Makes one pass, applying only appropriate desugarers
-- TODO: pull out the >>= f
walkTree :: LB.Expr -> DesugarM LB.Expr
walkTree expr = 
  let f = getDesugarer expr in
  case expr of 
    LB.ExprLet o var e1 e2 -> (LB.ExprLet o 
                                    <$> return var
                                    <*> walkTree e1
                                    <*> walkTree e2)
                                    >>= f
                                    
    LB.ExprScape o outerPattern e -> (LB.ExprScape o 
                                    <$> return outerPattern 
                                    <*> walkTree e)
                                    >>= f
                                    
    LB.ExprBinaryOp o e1 op e2 -> (LB.ExprBinaryOp o 
                                    <$> walkTree e1 
                                    <*> return op
                                    <*> walkTree e2)
                                    >>= f
                                    
    LB.ExprOnion o e1 e2 -> (LB.ExprOnion o 
                                    <$> walkTree e1 
                                    <*> walkTree e2)
                                    >>= f
                                    
    LB.ExprAppl o e1 e2 -> (LB.ExprAppl o 
                                    <$> walkTree e1 
                                    <*> walkTree e2)
                                    >>= f
                                    
    LB.ExprLabelExp o label e1 -> (LB.ExprLabelExp o 
                                    <$> return label 
                                    <*> walkTree e1)
                                    >>= f
                                    
    LB.ExprRef o e -> (LB.ExprRef o <$> walkTree e) >>= f
    LB.ExprVar o var -> (LB.ExprVar o <$> return var) >>= f
    LB.ExprValInt o int -> (LB.ExprValInt o <$> return int) >>= f
    LB.ExprValEmptyOnion o -> (return $ LB.ExprValEmptyOnion o) >>= f

    LB.ExprCondition o e1 e2 e3 -> (LB.ExprCondition o
                                    <$> walkTree e1
                                    <*> walkTree e2
                                    <*> walkTree e3)
                                    >>= f
    LB.ExprSequence o e1 e2 -> (LB.ExprSequence o
                                    <$> walkTree e1
                                    <*> walkTree e2)
                                    >>= f
    LB.ExprList o e -> (LB.ExprList o
                                    <$> mapM walkTree e)
                                    >>= f

-- |Desugar (if e1 then e2 else e3)
-- For en' := desugar en
-- The expression becomes
-- ((`True () -> e2') & (`False () -> e3')) e1';;
desugarIf :: LB.Expr -> DesugarM LB.Expr
desugarIf expr =
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

desugarSeq :: LB.Expr -> DesugarM LB.Expr
desugarSeq expr =
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

desugarList :: LB.Expr -> DesugarM LB.Expr
desugarList expr = 
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
desugarIdentity :: LB.Expr -> DesugarM LB.Expr
desugarIdentity expr = 
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
          
nextFreshVar :: DesugarM LB.Var
nextFreshVar = do
  s <- get
  let varName = "v" ++ show (freshVarIdx s)
  put $ s { freshVarIdx = freshVarIdx s + 1 }
  return $ LB.Var (TB.ComputedOrigin []) varName
