{-# LANGUAGE TemplateHaskell #-}

module Language.TinyBangNested.ATranslator.Translator
( aTranslate
) where

import Control.Applicative

import Language.TinyBang.Ast as TBA
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger
import Language.TinyBangNested.Ast as TBN
import Language.TinyBangNested.ATranslator.Monad

$(loggingFunctions)

-- TODO: some early failure mechanism for stuff that this translation can
--       detect (e.g. open expressions)

-- |Performs A-translation of a TinyBang Nested expression.
aTranslate :: TBN.Expr -> TBA.Expr
aTranslate expr =
  postLog _debugI
    (\tbaAst -> display $
        text "A-translation completed:" <> line <>
        indent 2 (align $
          text "Original AST:  " <+> makeDoc expr <> line <>
          text "Translated AST:" <+> makeDoc tbaAst)
    ) $ TBA.Expr generated $ fst $ runATranslationM $ innerATranslate expr

-- PERF: define expressions using Seq instead of [] so tail append is cheaper

-- |A-translates an expression.
innerATranslate :: TBN.Expr -> ATranslationM ([TBA.Clause], TBA.Var)
innerATranslate e =
  -- TODO: propagate origin data from TBN expression to TBA expression
  case e of
    ExprLet _ x e1 e2 -> do
      x' <- transVar x
      (cls1,x1) <- innerATranslate e1
      (cls2,x2) <- innerATranslate e2
      return ( cls1 ++
               [ TBA.Clause generated x' $ TBA.Copy generated x1 ] ++
               cls2
             , x2
             )
    ExprScape _ pat expr -> do
      x <- freshVar
      pcls <- fst <$> innerATranslatePat pat
      ecls <- fst <$> innerATranslate expr
      let pat' = TBA.Pattern generated pcls
      let expr' = TBA.Expr generated ecls
      return ( [ TBA.Clause generated x $ TBA.Def generated $
                    VScape generated pat' expr'
               ]
             , x
             )
    ExprBinaryOp _ e1 op e2 ->
      twoSubexpressionsAndThen e1 e2 $ \x1 x2 ->
        let op' = case op of
                    TBN.OpIntPlus _ -> TBA.OpIntPlus
                    TBN.OpIntMinus _ -> TBA.OpIntMinus
                    TBN.OpIntEq _ -> TBA.OpIntEq
                    TBN.OpIntGreaterEq _ -> TBA.OpIntGreaterEq
                    TBN.OpIntLessEq _ -> TBA.OpIntLessEq
        in
        TBA.Builtin generated op' [x1,x2]
    ExprOnion _ e1 e2 ->
      twoSubexpressionsAndThen e1 e2 $ \x1 x2 ->
        TBA.Def generated $ VOnion generated x1 x2   
    ExprAppl _ e1 e2 ->
      twoSubexpressionsAndThen e1 e2 $ \x1 x2 ->
        TBA.Appl generated x1 x2
    ExprLabelExp _ n e' -> do
      (cls,x) <- innerATranslate e'
      x' <- freshVar
      return ( cls ++
               [ TBA.Clause generated x' $ TBA.Def generated $
                    VLabel generated (aTransLabel n) x
               ]
             , x'
             )
    ExprRef _ e' -> do
      (cls,x) <- innerATranslate e'
      x' <- freshVar
      return ( cls ++
               [ TBA.Clause generated x' $ TBA.Def generated $
                    VRef generated x
               ]
             , x'
             )
    ExprValInt _ n -> do
      x <- freshVar
      return ( [ TBA.Clause generated x $ TBA.Def generated $
                    VPrimitive generated $ VInt generated n
               ]
             , x
             )
    ExprVar _ x -> do
      x' <- freshVar
      x'' <- transVar x
      return ( [ TBA.Clause generated x' $ TBA.Copy generated x'' ]
             , x'
             )
    ExprValEmptyOnion _ -> do
      x <- freshVar
      return ( [ TBA.Clause generated x $ TBA.Def generated $
                    VEmptyOnion generated
               ]
             , x
             )
  where
    twoSubexpressionsAndThen :: TBN.Expr
                             -> TBN.Expr
                             -> (TBA.Var -> TBA.Var -> TBA.Redex)
                             -> ATranslationM ([TBA.Clause], TBA.Var)
    twoSubexpressionsAndThen e1 e2 f = do
      (cls1,x1) <- innerATranslate e1
      (cls2,x2) <- innerATranslate e2
      x' <- freshVar
      return ( cls1 ++
               cls2 ++
               [ TBA.Clause generated x' $ f x1 x2 ]
             , x'
             )

innerATranslatePat :: TBN.Pattern
                   -> ATranslationM ([TBA.PatternClause], TBA.Var)
innerATranslatePat pat =
  -- TODO: propagate origin data from TBN pattern to TBA pattern
  case pat of
    PrimitivePattern _ t -> do
      x <- freshVar
      return ( [ PatternClause generated x $ PPrimitive generated $ 
                    aTransPrimitiveType t ]
             , x
             )
    LabelPattern _ n p -> do
      (pcls, x) <- innerATranslatePat p
      x' <- freshVar
      return ( pcls ++
               [ PatternClause generated x' $
                    PLabel generated (aTransLabel n) x ]
             , x'
             )
    ConjunctionPattern _ p1 p2 -> do
      (pcls1, x1) <- innerATranslatePat p1
      (pcls2, x2) <- innerATranslatePat p2
      x' <- freshVar
      return ( pcls1 ++
               pcls2 ++
               [ PatternClause generated x' $
                    PConjunction generated x1 x2 ]
             , x'
             )
    TBN.EmptyPattern _ -> do
      x' <- freshVar
      return ( [ PatternClause generated x' $ PEmptyOnion generated ]
             , x'
             )
    VariablePattern _ x -> do
      x' <- transVar x
      return ( [ PatternClause generated x' $ PEmptyOnion generated ]
             , x'
             )
      
aTransLabel :: TBN.LabelName -> TBA.LabelName
aTransLabel = TBA.LabelName generated . TBN.unLabelName

aTransPrimitiveType :: TBN.PrimitiveType -> TBA.PrimitiveType
aTransPrimitiveType t = case t of
  TBN.PrimInt -> TBA.PrimInt
