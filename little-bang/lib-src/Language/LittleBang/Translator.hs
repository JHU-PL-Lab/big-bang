module Language.LittleBang.Translator
( desugarLittleBang,
) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import Language.LittleBang.Ast as LB
import Language.TinyBang.Ast as TB
import Language.TinyBangNested.Ast as TBN
import Language.LittleBang.Lifter
import Language.TinyBang.Utils.Syntax.Location (SourceDocument(UnknownDocument))
import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBangNested.Syntax.Parser

{-
  TODO: this system needs to be restructured.  In general, the translator should
  make use of a single deep substitution function.  Each of the desugarings
  should be written as shallow specializations.
-}

-- | Desugar LittleBang. Do nothing for now
desugarLittleBang :: LB.Expr -> Either DesugarError LB.Expr
desugarLittleBang expr =
  let desugarAllExpr = foldl1 (>=>) exprDesugarers in -- Note: there is a monoid alternative
  let desugarAllPat = foldl1 (>=>) patDesugarers in
  let desugarContext = DesugarContext
        { desugarExprFn = desugarAllExpr,
          desugarPatFn  = desugarAllPat
        } in
  runDesugarM desugarContext $ walkExprTree expr
  --runDesugarM (return expr >>= walkExprTree)
  where
    -- First desugarer in the list will happen first.
    exprDesugarers :: [DesugarFunction LB.Expr]
    exprDesugarers =
      [ desugarExprIf
      , desugarExprObject
      , desugarExprClass
      , desugarExprList
      , desugarExprRecord
      , desugarExprProjection
      , desugarExprDispatch
      , desugarExprLScape
      , desugarExprLAppl
      , desugarExprDeref
      , desugarLExprBinaryOp
      , desugarLExprIndexedList
      ]
    patDesugarers :: [DesugarFunction LB.Pattern]
    patDesugarers =
      [ desugarPatList
      , desugarPatCons
      ]

runDesugarM :: DesugarContext -> DesugarM a -> Either DesugarError a
runDesugarM ctx x = fst <$> runStateT (runReaderT x ctx) (DesugarState 0)

type DesugarM = ReaderT (DesugarContext) (StateT DesugarState (Either DesugarError))
type DesugarError = String

type DesugarFunction a = a -> DesugarM a

data DesugarState = DesugarState { freshVarIdx :: Int }
data DesugarContext
  = DesugarContext
      { desugarExprFn :: DesugarFunction LB.Expr
      , desugarPatFn :: DesugarFunction LB.Pattern
      }

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
    LB.TExprGetChar o -> f (LB.TExprGetChar o)                             
    LB.TExprPutChar o e -> (LB.TExprPutChar o 
                                    <$> walkExprTree e) 
                                    >>= f        
    LB.TExprRef o e -> (LB.TExprRef o <$> walkExprTree e) >>= f
    LB.LExprDeref o e -> (LB.LExprDeref o <$> walkExprTree e) >>= f
    LB.TExprVar o var -> (LB.TExprVar o <$> return var) >>= f
    LB.TExprValInt o int -> (LB.TExprValInt o <$> return int) >>= f
    LB.TExprValChar o char -> (LB.TExprValChar o <$> return char) >>= f
    LB.TExprValEmptyOnion o -> f (LB.TExprValEmptyOnion o)
    
    LB.LExprScape o params e -> (LB.LExprScape o
                                 <$> mapM walkParamTree params
                                 <*> walkExprTree e)
                                >>= f

    LB.LExprAppl o e args -> (LB.LExprAppl o
                              <$> walkExprTree e
                              <*> mapM walkArgTree args)
                             >>= f

    LB.LExprCondition o e1 e2 e3 -> (LB.LExprCondition o
                                    <$> walkExprTree e1
                                    <*> walkExprTree e2
                                    <*> walkExprTree e3)
                                    >>= f
    LB.LExprBinaryOp o e1 op e2 -> (LB.LExprBinaryOp o
                                    <$> walkExprTree e1
                                    <*> return op
                                    <*> walkExprTree e2)
                                    >>= f
    LB.LExprList o e -> (LB.LExprList o
                                    <$> mapM walkExprTree e)
                                    >>= f
    LB.LExprRecord o args -> (LB.LExprRecord o
                                    <$> mapM walkArgTree args)
                                    >>= f
    LB.LExprObject o tms -> (LB.LExprObject o
                                    <$> mapM walkObjTermTree tms)
                                    >>= f
    LB.LExprClass o pms tms -> (LB.LExprClass o
                                    <$> mapM walkParamTree pms
                                    <*> mapM walkObjTermTree tms)
                                    >>= f
    LB.LExprProjection o e i -> (LB.LExprProjection o
                                 <$> walkExprTree e
                                 <*> pure i)
                                 >>= f
    LB.LExprDispatch o e i args -> (LB.LExprDispatch o
                                    <$> walkExprTree e
                                    <*> pure i
                                    <*> mapM walkArgTree args)
                                    >>= f
    LB.LExprIndexedList o e i -> (LB.LExprIndexedList o
                                 <$> walkExprTree e
                                 <*> walkExprTree i)
                                 >>= f
-- |Walk a parameter, applying desugarers
walkParamTree :: LB.Param -> DesugarM LB.Param
walkParamTree p = case p of
  LB.Param o v pat -> LB.Param o v <$> walkPatTree pat

-- |Walk an argument, applying desugarers
walkArgTree :: LB.Arg -> DesugarM LB.Arg
walkArgTree a = case a of
  LB.PositionalArg o e -> LB.PositionalArg o <$> walkExprTree e
  LB.NamedArg o s e -> LB.NamedArg o s <$> walkExprTree e

-- |Walk the tree of patterns, applying desugarers
walkPatTree :: LB.Pattern -> DesugarM LB.Pattern
walkPatTree pat = do
  f <- desugarPatFn <$> ask
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
    LB.ConsPattern o p1 p2 -> (LB.ConsPattern o
                                    <$> walkPatTree p1
                                    <*> walkPatTree p2)
                                    >>= f
    LB.EmptyPattern o -> f (LB.EmptyPattern o)
    LB.VariablePattern o var -> (LB.VariablePattern o <$> return var) >>= f
    LB.ListPattern o p t -> (LB.ListPattern o
                                    <$> mapM walkPatTree p)
                                    <*> (case t of
                                            Just p' -> Just <$> walkPatTree p'
                                            Nothing -> return Nothing
                                        )
                                    >>= f
                                    
-- |Walk the tree of object terms.
walkObjTermTree :: LB.ObjectTerm -> DesugarM LB.ObjectTerm
walkObjTermTree term =
  case term of
    LB.ObjectMethod o n params e ->
      LB.ObjectMethod o n
        <$> mapM walkParamTree params
        <*> walkExprTree e
    LB.ObjectField o n e ->
      LB.ObjectField o n
        <$> walkExprTree e

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

desugarLExprBinaryOp :: LB.Expr -> DesugarM LB.Expr
desugarLExprBinaryOp expr =
  case expr of
    LB.LExprBinaryOp o e1 op e2 ->
      case op of
        LB.OpSeq _ ->
          desugarExprSequence expr
        LB.OpCons _ ->
          desugarExprCons expr
        -- _ -> return expr
    _ -> return expr

desugarExprSequence :: LB.Expr -> DesugarM LB.Expr
desugarExprSequence expr =
  case expr of
    LB.LExprBinaryOp _ e1 op e2 ->
      case op of
        LB.OpSeq o ->
          LB.TExprAppl o <$>
            (LB.TExprScape o
              (LB.LabelPattern o (LB.LabelName o "Seq") (LB.EmptyPattern o))
              <$> return e2) <*>
            (LB.TExprLabelExp o <$>
              return (LB.LabelName o "Seq") <*>
              return e1)
        _ -> return expr
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

-- Assumption: cons is used only on valid lists.
-- This allows for simple, non-recursive pattern-matching
-- to detect whether e2 is a list.
desugarExprCons :: LB.Expr -> DesugarM LB.Expr
desugarExprCons expr =
  case expr of
    LB.LExprBinaryOp _ e1 op e2 -> return $
      case op of
        LB.OpCons o ->
          LB.TExprAppl o
            (LB.TExprAppl o
              (LB.TExprScape o
                (LB.VariablePattern o (LB.Ident o "h"))
                (LB.TExprOnion o
                  (LB.TExprScape o -- `Hd _ & `Tl _ & t -> `Hd h & `Tl t
                    (LB.ConjunctionPattern o
                      (LB.ConjunctionPattern o
                        (LB.LabelPattern o (LB.LabelName o "Hd") 
                          (LB.VariablePattern o (LB.Ident o "_")))
                        (LB.LabelPattern o (LB.LabelName o "Tl") 
                          (LB.VariablePattern o (LB.Ident o "_")))
                      )
                      (LB.VariablePattern o (LB.Ident o "t"))
                    )
                    (LB.TExprOnion o
                      (LB.TExprLabelExp o (LB.LabelName o "Hd") 
                        (LB.TExprVar o (LB.Ident o "h")))
                      (LB.TExprLabelExp o (LB.LabelName o "Tl") 
                        (LB.TExprVar o (LB.Ident o "t")))
                    )
                  )
                  (LB.TExprScape o -- `Nil _ -> `Hd h & `Tl `Nil ()
                    (LB.LabelPattern o (LB.LabelName o "Nil") 
                      (LB.VariablePattern o (LB.Ident o "_")))
                    (LB.TExprOnion o
                      (LB.TExprLabelExp o (LB.LabelName o "Hd") 
                        (LB.TExprVar o (LB.Ident o "h")))
                      (LB.TExprLabelExp o (LB.LabelName o "Tl") 
                        (LB.TExprLabelExp o (LB.LabelName o "Nil") 
                          (LB.TExprValEmptyOnion o))
                      )
                    )    
                  )
                )
              )
              e1
            )
            e2
        _ -> expr                               
    _ -> return expr

desugarExprRecord :: LB.Expr -> DesugarM LB.Expr
desugarExprRecord expr =
  case expr of
    LB.LExprRecord _ args -> desugarArgs args
    _ -> return expr

-- |A routine which converts each object term into an onion component.  This
--  routine must prepare each object term for sealing; we therefore add a
--  `self parameter to methods as well as a parameter identifying them.
objectTermToExpr :: LB.ObjectTerm -> DesugarM LB.Expr
objectTermToExpr term = case term of
  LB.ObjectMethod o n params e -> do
    pat <- desugarParams params
    let orig = TB.ComputedOrigin [o]
    let tagPat = LB.LabelPattern orig methodTagLabelName $
                    LB.LabelPattern orig (methodNameToLabelName n) $
                      LB.EmptyPattern orig
    let selfPat = LB.LabelPattern orig selfLabelName $
                    LB.VariablePattern orig $ LB.Ident orig "self"
    let msgPat = conjoinPatternList [pat,tagPat,selfPat]
    return $ LB.TExprScape o msgPat e
  LB.ObjectField o n e ->
    return $ LB.TExprLabelExp o (paramNameToLabelName n) e

desugarExprObject :: LB.Expr -> DesugarM LB.Expr
desugarExprObject expr =
  case expr of
    LB.LExprObject o tms -> seal o =<< onionExprList <$> mapM objectTermToExpr tms
    _ -> return expr

{-    
  addSelf :: TB.Origin -> LB.RecordTerm -> LB.RecordTerm
  addSelf o tm =
    let selfPart = LB.LabelPattern o (LB.LabelName o "self") (LB.VariablePattern o (LB.Var o "self")) in
    let e = unTermExpr tm in
    case e of
      LB.TExprScape o' pat z -> LB.TermNativeExpr o' (LB.TExprScape o'
        (LB.ConjunctionPattern o' selfPart pat) z)
      _ -> tm
-}

desugarExprClass :: LB.Expr -> DesugarM LB.Expr
desugarExprClass expr =
  case expr of
    LB.LExprClass o pms tms ->
      do
        objTermExprs <- mapM objectTermToExpr tms
        getClassFn <- objectTermToExpr
            (LB.ObjectMethod o (LB.Ident o "getclass") []
                (LB.TExprVar o (LB.Ident o "theclass")))
        instObj <- seal o (onionExprList (getClassFn : objTermExprs))
        let selfClosure = LB.TExprLet o (LB.Ident o "theclass") (LB.TExprVar o (LB.Ident o "self")) instObj
        -- TODO allow for class methods with ~
        seal o =<< objectTermToExpr (LB.ObjectMethod o (LB.Ident o "new") pms selfClosure)
    _ -> return expr

desugarArgs :: [LB.Arg] -> DesugarM LB.Expr
desugarArgs args =
  -- TODO: modify to accept positional arguments
  onionExprList <$> mapM desugarArg args
  where
    desugarArg :: LB.Arg -> DesugarM LB.Expr
    desugarArg arg =
      case arg of
        PositionalArg _ _ ->
          error "Positional arguments currently not supported!"
        NamedArg o n e ->
          return $ LB.TExprLabelExp o (paramNameToLabelName n) e

desugarParams :: [LB.Param] -> DesugarM LB.Pattern
desugarParams params =
  -- TODO: modify to accept positional parameters
  conjoinPatternList <$> mapM desugarParam params
  where
    desugarParam :: LB.Param -> DesugarM LB.Pattern
    desugarParam p = case p of
      Param o n pat ->
        return $ LB.LabelPattern o (paramNameToLabelName n) $
          LB.ConjunctionPattern o pat $ LB.VariablePattern (originOf n) n

distinguishedSymbol :: Char
distinguishedSymbol = '$'

paramNameToLabelName :: LB.Ident -> LB.LabelName
paramNameToLabelName (LB.Ident o n) =
  LB.LabelName o $ userSpecifiedInternalName n

methodNameToLabelName :: LB.Ident -> LB.LabelName
methodNameToLabelName (LB.Ident o n) =
  LB.LabelName o $ userSpecifiedInternalName n

userSpecifiedInternalName :: String -> String
userSpecifiedInternalName = (distinguishedSymbol:)

systemInternalName :: String -> String
systemInternalName = (distinguishedSymbol:) . (distinguishedSymbol:)

methodTagLabelName :: LB.LabelName
methodTagLabelName = LB.LabelName generated $ systemInternalName "msg"

selfLabelName :: LB.LabelName
-- TODO: currently avoiding $$self because TinyBangNested parser for seal can't
--       handle it.
selfLabelName = LB.LabelName generated "self"

desugarExprLScape :: LB.Expr -> DesugarM LB.Expr
desugarExprLScape expr =
  case expr of
    LB.LExprScape o params body ->
      LB.TExprScape o <$> desugarParams params <*> pure body
    _ -> return expr

desugarExprLAppl :: LB.Expr -> DesugarM LB.Expr
desugarExprLAppl expr =
  case expr of
    LB.LExprAppl o efunc args ->
      LB.TExprAppl o efunc <$> desugarArgs args
    _ -> return expr

-- TODO: projection should have different syntax for method invocation and field
--       access now (since we're going so far as to distinguish them)
desugarExprProjection :: LB.Expr -> DesugarM LB.Expr
desugarExprProjection expr =
    case expr of
      -- TODO: reconsider origins used in this AST
        LB.LExprProjection o e i ->
          let scape = LB.TExprScape o
                        (LB.LabelPattern o
                          (paramNameToLabelName i)
                          (LB.VariablePattern o i)
                        )
                        (LB.TExprVar o i)
          in
          return $ LB.TExprAppl o scape e
        _ -> return expr

desugarExprDispatch :: LB.Expr -> DesugarM LB.Expr
desugarExprDispatch expr =
  case expr of
    LB.LExprDispatch o e i args -> do
      dargs <- desugarArgs args
      let msgTag =  LB.TExprLabelExp (originOf i) methodTagLabelName $
                      LB.TExprLabelExp (originOf i) (methodNameToLabelName i) $
                        LB.TExprValEmptyOnion (originOf i)
      let message = LB.TExprOnion o msgTag dargs 
      return $ LB.TExprAppl o e message
    _ -> return expr

desugarExprDeref :: LB.Expr -> DesugarM LB.Expr
desugarExprDeref expr =
  case expr of
    LB.LExprDeref o e1 -> 
      return $
      LB.TExprAppl o
        (LB.TExprScape o
          (LB.RefPattern o (LB.VariablePattern o (LB.Ident o "n")))
          (LB.TExprVar o (LB.Ident o "n"))
        )
        e1
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

desugarPatCons :: LB.Pattern -> DesugarM LB.Pattern
desugarPatCons pat = 
  case pat of
    LB.ConsPattern o p1 p2 -> return $ 
      LB.ConjunctionPattern o 
      (LB.LabelPattern o (LB.LabelName o "Hd") p1) 
      (LB.LabelPattern o (LB.LabelName o "Tl") p2)
    _ -> return pat

-- TODO: see why this is not working correctly.
desugarLExprIndexedList :: LB.Expr -> DesugarM LB.Expr
desugarLExprIndexedList e = 
  case e of
    LB.LExprIndexedList o e' i -> getIndex o e' i
    _ -> return e
    where
    getIndex :: TB.Origin -> LB.Expr -> LB.Expr -> DesugarM LB.Expr
    getIndex o e' i =
      walkExprTree $ -- call desugar here
        LB.LExprCondition o
          (LB.TExprBinaryOp o 
           (LB.TExprBinaryOp o i (TBN.OpIntPlus o) (LB.TExprValInt o 1)) 
           (TBN.OpIntLessEq o) 
           (LB.TExprValInt o 0)) -- TODO: change to i < 0 when < is available in the language.
          (LB.TExprLabelExp o  (LB.LabelName o "Nil") (LB.TExprValEmptyOnion o)) -- TODO: add "index out of bounds exception"; this is a temporary placeholder.
          (LB.TExprLet o
            (LB.Ident o "obj")
            (LB.LExprObject o [objTerm])
            (LB.LExprDispatch o (LB.TExprVar o (LB.Ident o "obj")) (LB.Ident o "getElement")
             [LB.NamedArg o (LB.Ident o "lst") e',
              LB.NamedArg o (LB.Ident o "index") i])
      )
      where
      objTerm = ObjectMethod o (LB.Ident o "getElement")
        [LB.Param o (LB.Ident o "lst") (LB.EmptyPattern o), 
         LB.Param o (LB.Ident o "index") (LB.PrimitivePattern o LB.PrimInt)]
        (LB.TExprLet o (LB.Ident o "f")
          (LB.TExprOnion o
             (LB.LExprScape o [LB.Param o (LB.Ident o "l") 
                (LB.ConjunctionPattern o 
                   (LB.LabelPattern o (LB.LabelName o "Hd") (LB.VariablePattern o (LB.Ident o "hd"))) 
                   (LB.LabelPattern o (LB.LabelName o "Tl") (LB.VariablePattern o (LB.Ident o "tl"))))]
                (LB.LExprCondition o 
                   (LB.TExprBinaryOp o (LB.TExprVar o (LB.Ident o "index")) (TBN.OpIntEq o) (LB.TExprValInt o 0)) 
                   (LB.TExprVar o (LB.Ident o "hd")) 
                   (LB.LExprDispatch o (LB.TExprVar o (LB.Ident o "self")) 
                      (LB.Ident o "getElement") 
                      [LB.NamedArg o (LB.Ident o "lst") (LB.TExprVar o (LB.Ident o "tl")),
                       LB.NamedArg o (LB.Ident o "index") 
                         (LB.TExprBinaryOp o (LB.TExprVar o (LB.Ident o "index")) 
                         (TBN.OpIntMinus o)
                         (LB.TExprValInt o 1))]))
             ) 
             (LB.LExprScape o [LB.Param o (LB.Ident o "l") 
                (LB.LabelPattern o (LB.LabelName o "Nil") (LB.EmptyPattern o))]
                (LB.TExprLabelExp o  (LB.LabelName o "Nil") (LB.TExprValEmptyOnion o)) -- TODO: add "index out of bounds exception"; this is a temporary placeholder.
             )
           )
           (LB.LExprAppl o (LB.TExprVar o (LB.Ident o "f"))
              [LB.NamedArg o (LB.Ident o "l") (LB.TExprVar o (LB.Ident o "lst"))])
        )

-- |Seal a given expression.  This function generates code which will apply the
--  seal function to the provided expression. 
seal :: TB.Origin -> LB.Expr -> DesugarM LB.Expr
seal o e = -- parse this function directly until we have a prelude/stdlib for seal to load from
  -- generating seal directly until we have a prelude or stdlib to use
  let src =
        "(fun body -> (fun wrapper -> fun arg -> wrapper wrapper arg) (fun this -> fun arg -> body (this this) arg)) " ++
        "(fun seal -> fun obj -> (fun msg -> obj (msg & `self (seal obj))) & obj)" in
  let eitherAst = do -- Either
          tokens <- lexTinyBangNested UnknownDocument src
          parseTinyBangNested UnknownDocument tokens
  in
  case eitherAst of
    Left msg ->
      -- This should never happen.
      -- TODO: generate an appropriate error once DesugarM supports desugaring
      --       failures.
      error $ "Failed to parse seal function: " ++ msg
    Right sealExpr ->
      return $ LB.TExprAppl o (tbnLift sealExpr) e
          
nextFreshVar :: DesugarM LB.Ident
nextFreshVar = do
  s <- get
  let varName = 'v' : show (freshVarIdx s)
  put $ s { freshVarIdx = freshVarIdx s + 1 }
  return $ LB.Ident (TB.ComputedOrigin []) varName

conjoinPatterns :: LB.Pattern -> LB.Pattern -> LB.Pattern
conjoinPatterns p1 p2 =
  LB.ConjunctionPattern (originOf p1 <==> originOf p2) p1 p2
  
conjoinPatternList :: [LB.Pattern] -> LB.Pattern
conjoinPatternList pats =
  if null pats
    then LB.EmptyPattern generated
    else foldl1 conjoinPatterns pats

onionExprs :: LB.Expr -> LB.Expr -> LB.Expr
onionExprs e1 e2 =
  LB.TExprOnion (originOf e1 <==> originOf e2) e1 e2

onionExprList :: [LB.Expr] -> LB.Expr
onionExprList exprs =
  if null exprs
    then LB.TExprValEmptyOnion generated
    else foldl1 onionExprs exprs
