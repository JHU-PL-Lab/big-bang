module Language.LittleBang.Translator
( desugarLittleBang,
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBang.Ast as TB
import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Language.TinyBang.Utils.Syntax.Location (SourceDocument(UnknownDocument))
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Either

{-
  TODO: this system needs to be restructured.  In general, the translator should
  make use of a single deep substitution function.  Each of the desugarings
  should be written as shallow specializations.
-}

-- | Desugar LittleBang. Do nothing for now
desugarLittleBang :: LB.Expr -> Either DesugarError LB.Expr
desugarLittleBang expr =
  let desugarAllExpr = foldl1 (>=>) exprDesugarers in -- Note: there is a monoid alternative
  let desugarAllArg = foldl1 (>=>) argDesugarers in
  let desugarContext = DesugarContext
        { desugarExprFn = desugarAllExpr,
          desugarArgFn  = desugarAllArg
        } in
  runDesugarM desugarContext $ walkExprTree expr
  --runDesugarM (return expr >>= walkExprTree)
  where
    exprDesugarers :: [DesugarFunction LB.Expr]
    exprDesugarers =
      [
        desugarExprIf
      , desugarExprObject
      , desugarExprSeq
      , desugarExprList
      , desugarExprRecord
      , desugarExprProjection
      ]
    argDesugarers :: [DesugarFunction LB.RecordArgument]
    argDesugarers =
      [
        desugarArgIdent
      , desugarArgPat
      ]

runDesugarM :: DesugarContext -> DesugarM a -> Either DesugarError a
runDesugarM ctx x = fst <$> runStateT (runReaderT x ctx) (DesugarState 0)

type DesugarM = ReaderT (DesugarContext) (StateT DesugarState (Either DesugarError))
type DesugarError = String

type DesugarFunction a = a -> DesugarM a

data DesugarState = DesugarState { freshVarIdx :: Int }
data DesugarContext = DesugarContext { desugarExprFn :: DesugarFunction LB.Expr, desugarArgFn :: DesugarFunction LB.RecordArgument }

{-
-- | Get an expr desugaring function based on an AST node
getExprDesugarer :: LB.Expr -> DesugarFunction LB.Expr
getExprDesugarer expr =
  case expr of
    LB.LExprCondition _ _ _ _ -> desugarExprIf
    LB.LExprSequence _ _ _ -> desugarExprSeq
    LB.LExprList _ _ -> desugarExprList
    LB.LExprRecord _ _ -> desugarExprRecord
    LB.LExprObject o _ -> desugarObj o
    LB.LExprProjection _ _ _ -> desugarExprProjection
    _ -> desugarExprIdentity
    where -- TODO why didn't this work inline?
    desugarObj o e = do
        e1 <- desugarExprObject e
        e2 <- desugarExprRecord e1
        seal o e2
-}

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
    _ -> desugarTermIdentity

-- | Apply desugarers to the entire AST
-- Makes one pass, applying only appropriate desugarers
-- TODO: pull out the >>= f
walkExprTree :: LB.Expr -> DesugarM LB.Expr
walkExprTree expr = do
  f <- desugarExprFn <$> ask
  case expr of 
    LB.TExprLet o var e1 e2 -> (LB.TExprLet o 
                                    <$> return var
                                    <*> walkExprTree e1
                                    <*> walkExprTree e2)
                                    >>= f
                                    
    LB.TExprScape o outerPattern e -> (LB.TExprScape o 
                                    <$> walkPatTree outerPattern 
                                    <*> walkExprTree e)
                                    >>= f
                                    
    LB.TExprBinaryOp o e1 op e2 -> (LB.TExprBinaryOp o 
                                    <$> walkExprTree e1 
                                    <*> return op
                                    <*> walkExprTree e2)
                                    >>= f
                                    
    LB.TExprOnion o e1 e2 -> (LB.TExprOnion o 
                                    <$> walkExprTree e1 
                                    <*> walkExprTree e2)
                                    >>= f
                                    
    LB.TExprAppl o e1 e2 -> (LB.TExprAppl o 
                                    <$> walkExprTree e1 
                                    <*> walkExprTree e2)
                                    >>= f
                                    
    LB.TExprLabelExp o label e1 -> (LB.TExprLabelExp o 
                                    <$> return label 
                                    <*> walkExprTree e1)
                                    >>= f
                                    
    LB.TExprRef o e -> (LB.TExprRef o <$> walkExprTree e) >>= f
    LB.TExprVar o var -> (LB.TExprVar o <$> return var) >>= f
    LB.TExprValInt o int -> (LB.TExprValInt o <$> return int) >>= f
    LB.TExprValEmptyOnion o -> (return $ LB.TExprValEmptyOnion o) >>= f

    LB.LExprCondition o e1 e2 e3 -> (LB.LExprCondition o
                                    <$> walkExprTree e1
                                    <*> walkExprTree e2
                                    <*> walkExprTree e3)
                                    >>= f
    LB.LExprSequence o e1 e2 -> (LB.LExprSequence o
                                    <$> walkExprTree e1
                                    <*> walkExprTree e2)
                                    >>= f
    LB.LExprList o e -> (LB.LExprList o
                                    <$> mapM walkExprTree e)
                                    >>= f
    LB.LExprRecord o e -> (LB.LExprRecord o
                                    <$> mapM walkTermTree e)
                                    >>= f
    LB.LExprObject o tms -> (LB.LExprObject o
                                    <$> mapM walkTermTree tms)
                                    >>= f
    LB.LExprProjection o e1 e2 -> (LB.LExprProjection o
                                    <$> walkExprTree e1
                                    <*> walkExprTree e2)
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
                                    <*> mapM walkArgTree ts
                                    <*> walkExprTree e)
                                    >>= f
    LB.TermAnon o ts e -> (LB.TermAnon o
                                    <$> mapM walkArgTree ts
                                    <*> walkExprTree e)
                                    >>= f
    _ -> return tm

walkArgTree :: DesugarFunction LB.RecordArgument
walkArgTree arg = do
  f <- desugarArgFn <$> ask
  case arg of
    LB.ArgIdent o label -> (LB.ArgIdent o <$> return label) >>= f
    LB.ArgPat o l p -> (LB.ArgPat o
                                    <$> return l
                                    <*> walkPatTree p)
                                    >>= f

-- |Desugar (if e1 then e2 else e3)
-- For en' := desugar en
-- The expression becomes
-- ((`True () -> e2') & (`False () -> e3')) e1';;
desugarExprIf :: LB.Expr -> DesugarM LB.Expr
desugarExprIf expr =
  case expr of
    LB.LExprCondition o e1 e2 e3 ->
      LB.TExprAppl o <$>
        (LB.TExprOnion o <$>
          (LB.TExprScape o 
            (LB.LabelPattern o (LB.LabelName o "True") (LB.EmptyPattern o))
            <$> return e2) <*>
          (LB.TExprScape o 
            (LB.LabelPattern o (LB.LabelName o "False") (LB.EmptyPattern o))
            <$> return e3)) <*>
        (return e1)    
    _ -> return expr

desugarExprSeq :: LB.Expr -> DesugarM LB.Expr
desugarExprSeq expr =
  case expr of
    LB.LExprSequence o e1 e2 -> 
      LB.TExprAppl o <$>
        (LB.TExprScape o
          (LB.LabelPattern o (LB.LabelName o "Seq") (LB.EmptyPattern o))
          <$> return e2) <*>
        (LB.TExprLabelExp o <$>
          return (LB.LabelName o "Seq") <*>
          return e1)
    _ -> return expr

desugarExprList :: LB.Expr -> DesugarM LB.Expr
desugarExprList expr = 
  case expr of
    LB.LExprList o list -> toHTList o list
    _ -> return expr
    where
    toHTList :: TB.Origin -> [LB.Expr] -> DesugarM LB.Expr
    toHTList o lst = case lst of
      [] -> return (LB.TExprLabelExp o (LB.LabelName o "Nil") (LB.TExprValEmptyOnion o))
      e:t -> LB.TExprOnion o
                <$> return (LB.TExprLabelExp o (LB.LabelName o "Hd") e)
                <*> (LB.TExprLabelExp o <$> (LB.LabelName o <$> return "Tl") <*> toHTList o t)

buildRecord :: TB.Origin -> [LB.RecordTerm] -> LB.Expr
buildRecord o terms = foldl (\a b -> LB.TExprOnion o a b) (unTermExpr $ head terms) (map unTermExpr $ tail terms)

desugarExprRecord :: LB.Expr -> DesugarM LB.Expr
desugarExprRecord expr =
  case expr of
    LB.LExprRecord o list -> return $ buildRecord o list
    _ -> return expr

desugarExprObject :: LB.Expr -> DesugarM LB.Expr
desugarExprObject expr =
  case expr of
    LB.LExprObject o tms -> seal o $ buildRecord o (map (addSelf o) tms)
    _ -> return expr
  where
  addSelf :: TB.Origin -> LB.RecordTerm -> LB.RecordTerm
  addSelf o tm =
    let selfPart = LB.LabelPattern o (LB.LabelName o "self") (LB.VariablePattern o (LB.Var o "self")) in
    let e = unTermExpr tm in
    case e of
      LB.TExprScape o pat z -> LB.TermNativeExpr o (LB.TExprScape o
        (LB.ConjunctionPattern o selfPart pat) z)
      _ -> tm

desugarExprProjection :: LB.Expr -> DesugarM LB.Expr
desugarExprProjection expr =
    case expr of
        LB.LExprProjection o e1 e2 ->
            let (LB.TExprVar _ (LB.Var _ n)) = e2 in 
            return $
            LB.TExprAppl o
                (LB.TExprOnion o
                    (LB.TExprScape o
                        (LB.LabelPattern o
                            (LB.LabelName o n)
                            (LB.VariablePattern o (LB.Var o n))
                        )
                        (LB.TExprVar o (LB.Var o n))
                    )
                    (LB.TExprScape o
                        (LB.EmptyPattern o)
                        (LB.TExprScape o
                            (LB.VariablePattern o (LB.Var o "arg"))
                            (LB.TExprAppl o
                                e1
                                (LB.TExprOnion o
                                    (LB.TExprLabelExp o
                                        (LB.LabelName o "_msg")
                                        (LB.TExprLabelExp o
                                            (LB.LabelName o n)
                                            (LB.TExprValEmptyOnion o)
                                        )
                                    )
                                    (LB.TExprVar o (LB.Var o "arg"))
                                )
                            )
                        )
                    )
                )
                e1
        _ -> return expr

-- |An identity desugarer which does nothing
-- This is applied to all elements of LB that are also in TBN
desugarExprIdentity :: LB.Expr -> DesugarM LB.Expr
desugarExprIdentity expr = 
  case expr of
    LB.TExprLet o var e1 e2 -> LB.TExprLet o 
                                    <$> return var 
                                    <*> return e1 
                                    <*> return e2
                                    
    LB.TExprScape o outerPattern e -> LB.TExprScape o 
                                    <$> return outerPattern 
                                    <*> return e
                                    
    LB.TExprBinaryOp o e1 op e2 -> LB.TExprBinaryOp o 
                                    <$> return e1 
                                    <*> return op
                                    <*> return e2
                                    
    LB.TExprOnion o e1 e2 -> LB.TExprOnion o 
                                    <$> return e1 
                                    <*> return e2
                                    
    LB.TExprAppl o e1 e2 -> LB.TExprAppl o 
                                    <$> return e1 
                                    <*> return e2
                                    
    LB.TExprLabelExp o label e1 -> LB.TExprLabelExp o 
                                    <$> return label 
                                    <*> return e1
                                    
    LB.TExprRef o e -> LB.TExprRef o <$> return e
    LB.TExprVar o var -> LB.TExprVar o <$> return var
    LB.TExprValInt o int -> LB.TExprValInt o <$> return int
    LB.TExprValEmptyOnion o -> return (LB.TExprValEmptyOnion o)
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
    LB.TermIdent o label expr -> LB.TermNativeExpr o <$> (LB.TExprLabelExp o <$> return label <*> return expr)
    _ -> return tm

buildArguments :: TB.Origin -> [LB.RecordArgument] -> LB.Pattern
buildArguments o args = 
  case args of
    [] -> LB.EmptyPattern o
    _  -> foldl (\a b -> LB.ConjunctionPattern o a b) (unArgPat $ head args) (map unArgPat $ tail args)

desugarTermScape :: LB.RecordTerm -> DesugarM LB.RecordTerm
desugarTermScape tm = 
  case tm of
    LB.TermScape o label args body -> 
        let identPart = LB.LabelPattern o (LB.LabelName o "_msg") (LB.LabelPattern o label (LB.EmptyPattern o)) in
        let argPart = buildArguments o args in
        let pat = LB.ConjunctionPattern o identPart argPart in
        return $ LB.TermNativeExpr o (LB.TExprScape o pat body)
    _ -> return tm

desugarTermAnon :: LB.RecordTerm -> DesugarM LB.RecordTerm
desugarTermAnon tm = 
  case tm of
    LB.TermAnon o args body -> 
        let pat = buildArguments o args in
        return $ LB.TermNativeExpr o (LB.TExprScape o pat body)
    _ -> return tm

-- |desugarers for RecordArgument
desugarArgIdent :: DesugarFunction LB.RecordArgument
desugarArgIdent arg = 
  case arg of
    LB.ArgIdent o l -> LB.ArgNativePat o <$> (LB.LabelPattern o <$>
                                                return l
                                                <*> (LB.VariablePattern o
                                                        <$> (LB.Var o
                                                            <$> (return $ LB.unLabelName l)
                                                        )
                                                    )
                                                )
    _ -> return arg

desugarArgPat :: DesugarFunction LB.RecordArgument
desugarArgPat arg = 
  case arg of
    LB.ArgPat o label p -> 
        let var = LB.VariablePattern o (LB.Var o (LB.unLabelName label)) in
            LB.ArgNativePat o <$> (LB.LabelPattern o
                <$> return label
                <*> (LB.ConjunctionPattern o <$> return var <*> return p))
    _ -> return arg

desugarTermIdentity :: LB.RecordTerm -> DesugarM LB.RecordTerm
desugarTermIdentity tm = return tm

unTermExpr (LB.TermNativeExpr _ e) = e
unArgPat  (LB.ArgNativePat  _ p) = p

-- seal function
seal :: TB.Origin -> LB.Expr -> DesugarM LB.Expr
seal o e = -- parse this function directly until we have a prelude/stdlib for seal to load from
    let src = "(f -> (g -> x -> g g x) (h -> y -> f (h h) y)) (seal -> obj -> (msg -> obj (msg & `self (seal obj))) & obj)" in
    let eitherAst = do -- Either
            tokens <- lexLittleBang UnknownDocument src
            parseLittleBang UnknownDocument tokens
    in return $ either (const e) (const (LB.TExprAppl o (head $ rights [eitherAst]) e)) eitherAst
          
nextFreshVar :: DesugarM LB.Var
nextFreshVar = do
  s <- get
  let varName = "v" ++ show (freshVarIdx s)
  put $ s { freshVarIdx = freshVarIdx s + 1 }
  return $ LB.Var (TB.ComputedOrigin []) varName
