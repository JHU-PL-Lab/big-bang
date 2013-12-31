{-# LANGUAGE FlexibleInstances, OverlappingInstances, GADTs #-}

-- See notes/TypeConstraint-Overlapping.txt for explanation of
-- OverlappingInstances 

module Language.TinyBang.TypeSystem.Relations.Contours.ContourReplacable
where -- TODO
{-
( ContourReplacable(..)
) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Data.Compatibility
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Types

-- | A typeclass for contour replacement.
class ContourReplacable a where
  replContours :: Contour -> a -> a
  
instance (Ord a, ContourReplacable a) => ContourReplacable (Set a) where
  replContours cn = Set.map $ replContours cn

instance (ContourReplacable a, Functor f) => ContourReplacable (f a) where
  replContours cn = fmap (replContours cn)
  
instance (ContourReplacable a) => ContourReplacable (Maybe a) where
  replContours cn mx = replContours cn <$> mx 

instance (ConstraintDatabase db) => ContourReplacable (Constraint db) where
  replContours cn wc = case wc of
    WrapIntermediateConstraint c
      -> WrapIntermediateConstraint $ replContours cn c
    WrapTypeConstraint c
      -> WrapTypeConstraint $ replContours cn c
    WrapApplicationConstraint c
      -> WrapApplicationConstraint $ replContours cn c
    WrapOperationConstraint c
      -> WrapOperationConstraint $ replContours cn c
    WrapCellCreationConstraint c
      -> WrapCellCreationConstraint $ replContours cn c
    WrapCellLoadingConstraint c
      -> WrapCellLoadingConstraint $ replContours cn c
    WrapCellSettingConstraint c
      -> WrapCellSettingConstraint $ replContours cn c
    WrapFinalConstraint c
      -> WrapFinalConstraint $ replContours cn c
    WrapImmutableConstraint c
      -> WrapImmutableConstraint $ replContours cn c
    WrapFlowConstraint c
      -> WrapFlowConstraint $ replContours cn c
    WrapExceptionConstraint c
      -> WrapExceptionConstraint $ replContours cn c

instance ContourReplacable IntermediateConstraint where
  replContours cn (IntermediateConstraint a a') = 
      IntermediateConstraint (replContours cn a) (replContours cn a')

instance (ConstraintDatabase db) => ContourReplacable (TypeConstraint db) where
  replContours cn (TypeConstraint t a) = 
      TypeConstraint (replContours cn t) (replContours cn a)

instance ContourReplacable ApplicationConstraint where
  replContours cn (ApplicationConstraint a1 a2 a3) = 
      ApplicationConstraint (replContours cn a1) (replContours cn a2)
        (replContours cn a3)

instance ContourReplacable OperationConstraint where
  replContours cn (OperationConstraint a1 op a2 a3) = 
      OperationConstraint (replContours cn a1) op (replContours cn a2)
        (replContours cn a3)

instance ContourReplacable CellCreationConstraint where
  replContours cn (CellCreationConstraint a b) = 
      CellCreationConstraint (replContours cn a) (replContours cn b)

instance ContourReplacable CellLoadingConstraint where
  replContours cn (CellLoadingConstraint b a) = 
      CellLoadingConstraint (replContours cn b) (replContours cn a)

instance ContourReplacable CellSettingConstraint where
  replContours cn (CellSettingConstraint a b) = 
      CellSettingConstraint (replContours cn a) (replContours cn b)

instance ContourReplacable FinalConstraint where
  replContours cn (FinalConstraint a) = 
      FinalConstraint $ replContours cn a

instance ContourReplacable ImmutableConstraint where
  replContours cn (ImmutableConstraint a) = 
      ImmutableConstraint $ replContours cn a

instance ContourReplacable FlowConstraint where
  replContours cn (FlowConstraint a k a') = 
      FlowConstraint (replContours cn a) k (replContours cn a')

instance ContourReplacable ExceptionConstraint where
  replContours cn (ExceptionConstraint a a') = 
      ExceptionConstraint (replContours cn a) (replContours cn a')
      
instance (ConstraintDatabase db) => ContourReplacable (Type db) where
  replContours cn t = case t of
    Primitive _ -> t
    EmptyOnion -> t
    Label n b -> Label n $ replContours cn b
    Onion a a' -> Onion (replContours cn a) (replContours cn a')
    OnionFilter a op proj -> OnionFilter (replContours cn a) op proj
    Scape tpat a cs ->
      Scape (replContours cn tpat) (replContours cn a) (replaceContours cn cs)

instance ContourReplacable PatternType where
  replContours cn tpat = case tpat of
    ValuePattern b tipat ->
      ValuePattern (replContours cn b) (replContours cn tipat)
    ExnPattern b tipat ->
      ExnPattern (replContours cn b) (replContours cn tipat)
      
instance ContourReplacable InnerPatternType where
  replContours cn tipat = case tipat of
    PrimitivePattern _ -> tipat
    LabelPattern n b tipat' ->
      LabelPattern n (replContours cn b) (replContours cn tipat')
    ConjunctivePattern tipat' tipat'' ->
      ConjunctivePattern (replContours cn tipat') (replContours cn tipat'')
    ScapePattern -> tipat
    EmptyOnionPattern -> tipat

instance ContourReplacable FlowTVar where
  replContours cn a = case a of
    FlowTVar x pcn -> FlowTVar x $ replContours cn pcn
    GenFlowTVar x pcn -> GenFlowTVar x $ replContours cn pcn

instance ContourReplacable CellTVar where
  replContours cn a = case a of
    CellTVar x pcn -> CellTVar x $ replContours cn pcn
    GenCellTVar x pcn -> GenCellTVar x $ replContours cn pcn

instance ContourReplacable PossibleContour where
  replContours cn (PossibleContour mcn) = PossibleContour $ replContours cn mcn

instance ContourReplacable Contour where
  replContours cn cn' = if cn' `subsumedBy` cn then cn else cn'

instance (ConstraintDatabase db, ContourReplacable db)
      => ContourReplacable (ConstraintHistory db) where
  replContours cn hist = case hist of
    DerivedFromSource _ -> hist
    CompatibilityWiring -> hist
    DerivedFromClosure rule -> DerivedFromClosure $ replContours cn rule

instance (ConstraintDatabase db, ContourReplacable db)
      => ContourReplacable (ClosureRule db) where
  replContours cn arg = case arg of
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
      rec :: (ContourReplacable a) => a -> a
      rec = replContours cn

instance (ConstraintDatabase db)
      => ContourReplacable (ProjectionResult db) where
  replContours cn (ProjectionResult proj a form) =
    ProjectionResult proj (replContours cn a) (replContours cn form)

instance (ConstraintDatabase db)
      => ContourReplacable (ProjectionResultForm db tag) where
  replContours cn form = case form of
    ProjectionResultPrimForm x f ->
      ProjectionResultPrimForm x (replContours cn f)
    ProjectionResultLabelForm bs f ->
      ProjectionResultLabelForm (replContours cn bs) (replContours cn f)
    ProjectionResultFunForm ts f ->
      ProjectionResultFunForm (replContours cn ts) (replContours cn f)
        
instance (ConstraintDatabase db, ContourReplacable db)
      => ContourReplacable (ApplicationCompatibilityResult db) where
  replContours cn (ApplicationCompatibilityResult ca ts mdat fib) =
    ApplicationCompatibilityResult (replContours cn ca)
      (replContours cn ts) (replContours cn mdat) (replContours cn fib)
      
instance ContourReplacable CellLowerBoundingConstraint where
  replContours cn lbc = case lbc of
    CellCreationLowerBoundingConstraint c ->
      CellCreationLowerBoundingConstraint $ replContours cn c
    CellSettingLowerBoundingConstraint c ->
      CellSettingLowerBoundingConstraint $ replContours cn c
      
instance (ConstraintDatabase db)
      => ContourReplacable (Fibration db) where
  replContours cn fib = case fib of
    Unexpanded -> Unexpanded
    Fibration t fs -> Fibration (replContours cn t) (replContours cn fs)
    
instance ContourReplacable CompatibilityArgument where
  replContours cn ca = case ca of
    ArgVal a -> ArgVal $ replContours cn a
    ArgExn a -> ArgExn $ replContours cn a
-}