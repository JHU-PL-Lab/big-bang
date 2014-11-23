{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Language.TinyBangNested.ATranslator.Translator
( aTranslate
) where

import Control.Applicative
import Control.Arrow hiding ((<+>))
import qualified Data.Map as Map
import Data.Sequence (Seq, (|>), (><), ViewL(..))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

import Language.TinyBang.Ast as TBA
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger
import Language.TinyBangNested.Ast as TBN
import Language.TinyBangNested.ATranslator.Monad

$(loggingFunctions)

-- TODO: some early failure mechanism for stuff that this translation can
--       detect

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
      (cls1,x1) <- innerATranslate e1
      (x', (cls2,x2)) <- bracketScope $
                            (,) <$> bindVar x <*> innerATranslate e2
      return ( cls1 ++
               [ TBA.Clause generated x' $ TBA.Copy generated x1 ] ++
               cls2
             , x2
             )
    ExprScape _ pat expr -> do
      x <- freshVar
      ((pfcs,px), ecls) <- bracketScope $
                            (,) <$>
                              innerATranslatePat pat <*>
                              (fst <$> innerATranslate expr)
      let pat' = TBA.Pattern generated px $ PatternFilterMap $
                    Map.fromList $ map (second (generated,)) $
                    Foldable.toList pfcs
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
                    TBN.OpSet _ -> TBA.OpSet
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
    ExprValChar _ n -> do
      x <- freshVar
      return ( [ TBA.Clause generated x $ TBA.Def generated $
                    VPrimitive generated $ VChar generated n
               ]
             , x
             )
    ExprVar _ x -> do
      x' <- freshVar
      x'' <- useVar x
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
    ExprGetChar _ -> do
      x <- freshVar
      return ( [ TBA.Clause generated x $ TBA.GetChar generated
               ]
             , x
             )
    ExprPutChar _ e' -> do
      x <- freshVar
      (cls,x') <- innerATranslate e'
      return ( cls ++
               [ TBA.Clause generated x $ TBA.PutChar generated x'
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

-- |Generates a list of ANF pattern mappings for a given pattern.  The ANF
--  pattern which should be created is rooted at the rightmost variable in the
--  sequence.
innerATranslatePat :: TBN.Pattern
                   -> ATranslationM (Seq (TBA.Var, Filter), TBA.Var)
innerATranslatePat pat = do
  -- TODO: propagate origin data from TBN pattern to TBA pattern
  s <- case pat of
        PrimitivePattern _ t -> do
          x <- freshVar
          return $ Seq.singleton
                    (x,FPrimitive generated $ aTransPrimitiveType t)
        LabelPattern _ n p -> do
          (pfcs, x) <- innerATranslatePat p
          x' <- freshVar
          return $ pfcs |> (x',FLabel generated (aTransLabel n) x)
        RefPattern _ p -> do
          (pfcs, x) <- innerATranslatePat p
          x' <- freshVar
          return $ pfcs |> (x',FRef generated x)
        ConjunctionPattern _ p1 p2 -> do
          (pfcs1, x1) <- innerATranslatePat p1
          (pfcs2, x2) <- innerATranslatePat p2
          x' <- freshVar
          return $ (pfcs1 >< pfcs2) |> (x',FConjunction generated x1 x2)
        TBN.EmptyPattern _ -> do
          x' <- freshVar
          return $ Seq.singleton (x',FEmptyOnion generated)
        VariablePattern _ x -> do
          x' <- bindVar x
          return $ Seq.singleton (x',FEmptyOnion generated)
  return (s, headVarOf s)
{-
  in Pattern generated x $ PatternFilterMap $ Map.fromList $
        map (second (generated,)) terms
-}

headVarOf :: Seq (TBA.Var, a) -> TBA.Var
headVarOf s =
  case Seq.viewl s of
    Seq.EmptyL -> error "Empty pattern term sequence!"
    x :< _ -> fst x
      
aTransLabel :: TBN.LabelName -> TBA.LabelName
aTransLabel = TBA.LabelName generated . TBN.unLabelName

aTransPrimitiveType :: TBN.PrimitiveType -> TBA.PrimitiveType
aTransPrimitiveType t = case t of
  TBN.PrimInt -> TBA.PrimInt
  TBN.PrimChar -> TBA.PrimChar
