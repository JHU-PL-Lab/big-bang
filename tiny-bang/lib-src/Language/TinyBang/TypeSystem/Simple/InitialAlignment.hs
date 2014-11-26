{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Language.TinyBang.TypeSystem.Simple.InitialAlignment
( initiallyAlign
, initiallyAlignVar
) where

import Control.Arrow
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Simple.Data
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

initiallyAlign :: Expr -> (TVar, ConstraintSet)
initiallyAlign (Expr _ cls) =
  let alignments = map initiallyAlignClause cls in
  (fst $ last alignments, mconcat $ map (ConstraintSet . Set.singleton . snd) alignments)

initiallyAlignClause :: Clause -> (TVar, Constraint)
initiallyAlignClause (Clause _ x r) =
  let a = initiallyAlignVar x in (a,) $
  case r of
    Def _ v -> initiallyAlignValue v <: a
    Copy _ x' -> initiallyAlignVar x' <: a
    Appl _ x' x'' -> (initiallyAlignVar x', initiallyAlignVar x'') <: a
    Builtin _ op xs -> BuiltinOpConstraint op (map initiallyAlignVar xs) a
    GetChar _ -> FilteredType (TPrimitive PrimChar) mempty mempty <: a
    PutChar _ x' -> error "Language.TinyBang.TypeSystem.Simple.InitialAlignment undefined" -- TODO

initiallyAlignValue :: Value -> FilteredType
initiallyAlignValue v = 
  let t = case v of
            VPrimitive _ pv -> TPrimitive $ case pv of
              VInt _ _ -> PrimInt
              VChar _ _ -> PrimChar
            VEmptyOnion _ -> TEmptyOnion
            VLabel _ n x -> TLabel n $ initiallyAlignVar x
            VRef _ x -> TRef $ initiallyAlignVar x
            VOnion _ x1 x2 -> (TOnion `on` initiallyAlignVar) x1 x2
            VScape _ p e ->
              let pt = initiallyAlignPattern p in
              let (a,cs) = initiallyAlign e in
              TScape pt a cs
  in
  FilteredType t mempty mempty

initiallyAlignVar :: Var -> TVar
initiallyAlignVar x = TVar x $ PossibleContour Nothing

initiallyAlignPattern :: Pattern -> PatternType
initiallyAlignPattern (Pattern _ x pfm) =
  PatternType (initiallyAlignVar x) $ initiallyAlignPatternFilterMap pfm

initiallyAlignPatternFilterMap :: PatternFilterMap -> Map TVar FilterType
initiallyAlignPatternFilterMap (PatternFilterMap pfm) =
  let entries = map (second snd) $ Map.toList pfm in
  let tentries =
        map (initiallyAlignVar *** initiallyAlignPatternFilter) entries in
  Map.fromList tentries

initiallyAlignPatternFilter :: Filter -> FilterType
initiallyAlignPatternFilter f =
  case f of
    FPrimitive _ pt -> TFPrim pt
    FEmptyOnion _ -> TFEmpty
    FLabel _ n x -> TFLabel n $ initiallyAlignVar x
    FRef _ x -> TFRef $ initiallyAlignVar x
    FConjunction _ x1 x2 -> (TFConjunction `on` initiallyAlignVar) x1 x2

