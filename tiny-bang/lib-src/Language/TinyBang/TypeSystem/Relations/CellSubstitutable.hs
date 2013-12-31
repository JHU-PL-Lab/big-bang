{-# LANGUAGE FlexibleInstances, OverlappingInstances, GADTs #-}

-- See notes/TypeConstraint-Overlapping.txt for explanation of
-- OverlappingInstances 

{-|
  This module defines a homomorphic relation for substituting cells throughout
  type system structures.
-}
module Language.TinyBang.TypeSystem.Relations.CellSubstitutable
where -- TODO
{-
( CellSubstitutable(..)
) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Data.Compatibility
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Types

class CellSubstitutable a where
  substCells :: Map CellTVar CellTVar -> a -> a

instance (Ord a, CellSubstitutable a) => CellSubstitutable (Set a) where
  substCells m = Set.map $ substCells m

instance (CellSubstitutable a, Functor f) => CellSubstitutable (f a) where
  substCells m = fmap (substCells m)
  
instance (CellSubstitutable a) => CellSubstitutable (Maybe a) where
  substCells m mx = substCells m <$> mx 

instance (ConstraintDatabase db) => CellSubstitutable (Constraint db) where
  substCells m wc = case wc of
    WrapIntermediateConstraint c
      -> WrapIntermediateConstraint $ substCells m c
    WrapTypeConstraint c
      -> WrapTypeConstraint $ substCells m c
    WrapApplicationConstraint c
      -> WrapApplicationConstraint $ substCells m c
    WrapOperationConstraint c
      -> WrapOperationConstraint $ substCells m c
    WrapCellCreationConstraint c
      -> WrapCellCreationConstraint $ substCells m c
    WrapCellLoadingConstraint c
      -> WrapCellLoadingConstraint $ substCells m c
    WrapCellSettingConstraint c
      -> WrapCellSettingConstraint $ substCells m c
    WrapFinalConstraint c
      -> WrapFinalConstraint $ substCells m c
    WrapImmutableConstraint c
      -> WrapImmutableConstraint $ substCells m c
    WrapFlowConstraint c
      -> WrapFlowConstraint $ substCells m c
    WrapExceptionConstraint c
      -> WrapExceptionConstraint $ substCells m c

instance CellSubstitutable IntermediateConstraint where
  substCells m (IntermediateConstraint a a') = 
      IntermediateConstraint (substCells m a) (substCells m a')

instance (ConstraintDatabase db) => CellSubstitutable (TypeConstraint db) where
  substCells m (TypeConstraint t a) = 
      TypeConstraint (substCells m t) (substCells m a)

instance CellSubstitutable ApplicationConstraint where
  substCells m (ApplicationConstraint a1 a2 a3) = 
      ApplicationConstraint (substCells m a1) (substCells m a2)
        (substCells m a3)

instance CellSubstitutable OperationConstraint where
  substCells m (OperationConstraint a1 op a2 a3) = 
      OperationConstraint (substCells m a1) op (substCells m a2)
        (substCells m a3)

instance CellSubstitutable CellCreationConstraint where
  substCells m (CellCreationConstraint a b) = 
      CellCreationConstraint (substCells m a) (substCells m b)

instance CellSubstitutable CellLoadingConstraint where
  substCells m (CellLoadingConstraint b a) = 
      CellLoadingConstraint (substCells m b) (substCells m a)

instance CellSubstitutable CellSettingConstraint where
  substCells m (CellSettingConstraint a b) = 
      CellSettingConstraint (substCells m a) (substCells m b)

instance CellSubstitutable FinalConstraint where
  substCells m (FinalConstraint a) = 
      FinalConstraint $ substCells m a

instance CellSubstitutable ImmutableConstraint where
  substCells m (ImmutableConstraint a) = 
      ImmutableConstraint $ substCells m a

instance CellSubstitutable FlowConstraint where
  substCells m (FlowConstraint a k a') = 
      FlowConstraint (substCells m a) k (substCells m a')

instance CellSubstitutable ExceptionConstraint where
  substCells m (ExceptionConstraint a a') = 
      ExceptionConstraint (substCells m a) (substCells m a')
      
instance (ConstraintDatabase db) => CellSubstitutable (Type db) where
  substCells m t = case t of
    Primitive _ -> t
    EmptyOnion -> t
    Label n b -> Label n $ substCells m b
    Onion a a' -> Onion (substCells m a) (substCells m a')
    OnionFilter a op proj -> OnionFilter (substCells m a) op proj
    Scape tpat a cs ->
      Scape (substCells m tpat) (substCells m a) (substituteCellVariables m cs)

instance CellSubstitutable PatternType where
  substCells m tpat = case tpat of
    ValuePattern b tipat ->
      ValuePattern (substCells m b) (substCells m tipat)
    ExnPattern b tipat ->
      ExnPattern (substCells m b) (substCells m tipat)
      
instance CellSubstitutable InnerPatternType where
  substCells m tipat = case tipat of
    PrimitivePattern _ -> tipat
    LabelPattern n b tipat' ->
      LabelPattern n (substCells m b) (substCells m tipat')
    ConjunctivePattern tipat' tipat'' ->
      ConjunctivePattern (substCells m tipat') (substCells m tipat'')
    ScapePattern -> tipat
    EmptyOnionPattern -> tipat

instance CellSubstitutable FlowTVar where
  substCells = const id

instance CellSubstitutable CellTVar where
  substCells m a = fromMaybe a $ Map.lookup a m

instance (ConstraintDatabase db, CellSubstitutable db)
      => CellSubstitutable (ConstraintHistory db) where
  substCells m hist = case hist of
    DerivedFromSource _ -> hist
    CompatibilityWiring -> hist
    DerivedFromClosure rule -> DerivedFromClosure $ substCells m rule

instance (ConstraintDatabase db, CellSubstitutable db)
      => CellSubstitutable (ClosureRule db) where
  substCells m arg = case arg of
    TransitivityRule tc ic -> TransitivityRule (rec tc) (rec ic)
    IntegerOperationRule oc pr1 pr2 -> IntegerOperationRule (rec oc) (rec pr1)
                                          (rec pr2)
    IntegerCalculationRule oc pr1 pr2 -> IntegerCalculationRule (rec oc)
                                            (rec pr1) (rec pr2)
    EqualityRule oc -> EqualityRule $ rec oc
    ApplicationRule ac mpr acr cn' ->
      ApplicationRule (rec ac) (rec mpr) (rec acr) cn'
    CellPropagationRule lc lbc -> CellPropagationRule (rec lc) (rec lbc)
    ExceptionPropagationRule ec fc -> ExceptionPropagationRule (rec ec) (rec fc)
    ExceptionPassRule ac ec mpr acr ->
      ExceptionPassRule (rec ac) (rec ec) (rec mpr) (rec acr)
    ExceptionCatchRule ac ec mpr acr cn' ->
      ExceptionCatchRule (rec ac) (rec ec) (rec mpr) (rec acr) cn'
    where
      rec :: (CellSubstitutable a) => a -> a
      rec = substCells m

instance (ConstraintDatabase db)
      => CellSubstitutable (ProjectionResult db) where
  substCells m (ProjectionResult proj a result) =
    ProjectionResult proj (substCells m a) (substCells m result)
    
instance (ConstraintDatabase db)
      => CellSubstitutable (ProjectionResultForm db tag) where
  substCells m form = case form of
    ProjectionResultPrimForm x f ->
      ProjectionResultPrimForm x (substCells m f)
    ProjectionResultLabelForm bs f ->
      ProjectionResultLabelForm (substCells m bs) (substCells m f)
    ProjectionResultFunForm ts f ->
      ProjectionResultFunForm (substCells m ts) (substCells m f)
        
instance (ConstraintDatabase db, CellSubstitutable db)
      => CellSubstitutable (ApplicationCompatibilityResult db) where
  substCells m (ApplicationCompatibilityResult ca ts mdat fib) =
    ApplicationCompatibilityResult (substCells m ca)
      (substCells m ts) (substCells m mdat) (substCells m fib)
      
instance CellSubstitutable CellLowerBoundingConstraint where
  substCells m lbc = case lbc of
    CellCreationLowerBoundingConstraint c ->
      CellCreationLowerBoundingConstraint $ substCells m c
    CellSettingLowerBoundingConstraint c ->
      CellSettingLowerBoundingConstraint $ substCells m c
      
instance (ConstraintDatabase db)
      => CellSubstitutable (Fibration db) where
  substCells m fib = case fib of
    Unexpanded -> Unexpanded
    Fibration t fs -> Fibration (substCells m t) (substCells m fs)
    
instance CellSubstitutable CompatibilityArgument where
  substCells m ca = case ca of
    ArgVal a -> ArgVal $ substCells m a
    ArgExn a -> ArgExn $ substCells m a
-}