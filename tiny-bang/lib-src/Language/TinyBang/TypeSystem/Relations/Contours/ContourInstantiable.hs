{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

-- See notes/TypeConstraint-Overlapping.txt for explanation of
-- OverlappingInstances 

module Language.TinyBang.TypeSystem.Relations.Contours.ContourInstantiable
( ContourInstantiable(..)
) where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Data.Compatibility
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Types

-- | A typeclass for contour instantiation.
class ContourInstantiable a where
  instContours :: Set AnyTVar -> Contour -> a -> a

-- ---- Typeclass instances for contour instantiation

instance (Ord a, ContourInstantiable a) => ContourInstantiable (Set a) where
  instContours vs cn = Set.map $ instContours vs cn

instance (ContourInstantiable a, Functor f) => ContourInstantiable (f a) where
  instContours vs cn = fmap (instContours vs cn)
  
instance (ConstraintDatabase db) => ContourInstantiable (Constraint db) where
  instContours vs cn arg = case arg of
    WrapIntermediateConstraint c -> cwrap $ instContours vs cn c
    WrapTypeConstraint c -> cwrap $ instContours vs cn c
    WrapApplicationConstraint c -> cwrap $ instContours vs cn c
    WrapOperationConstraint c -> cwrap $ instContours vs cn c
    WrapCellCreationConstraint c -> cwrap $ instContours vs cn c
    WrapCellLoadingConstraint c -> cwrap $ instContours vs cn c
    WrapCellSettingConstraint c -> cwrap $ instContours vs cn c
    WrapFinalConstraint c -> cwrap $ instContours vs cn c
    WrapImmutableConstraint c -> cwrap $ instContours vs cn c
    WrapFlowConstraint c -> cwrap $ instContours vs cn c
    WrapExceptionConstraint c -> cwrap $ instContours vs cn c

instance ContourInstantiable IntermediateConstraint where
  instContours vs cn (IntermediateConstraint a a') =
    IntermediateConstraint (instContours vs cn a) (instContours vs cn a')
  
instance (ConstraintDatabase db) => ContourInstantiable (TypeConstraint db) where
  instContours vs cn (TypeConstraint t a) =
    TypeConstraint (instContours vs cn t) (instContours vs cn a)

instance ContourInstantiable ApplicationConstraint where
  instContours vs cn (ApplicationConstraint a a' a'') =
    ApplicationConstraint (instContours vs cn a) (instContours vs cn a')
      (instContours vs cn a'') 

instance ContourInstantiable OperationConstraint where
  instContours vs cn (OperationConstraint a op a' a'') = 
    OperationConstraint (instContours vs cn a) op (instContours vs cn a')
      (instContours vs cn a'') 

instance ContourInstantiable CellCreationConstraint where
  instContours vs cn (CellCreationConstraint a b) =
    CellCreationConstraint (instContours vs cn a) (instContours vs cn b)

instance ContourInstantiable CellLoadingConstraint where
  instContours vs cn (CellLoadingConstraint b a) =
    CellLoadingConstraint (instContours vs cn b) (instContours vs cn a)

instance ContourInstantiable CellSettingConstraint where
  instContours vs cn (CellSettingConstraint a b) =
    CellSettingConstraint (instContours vs cn a) (instContours vs cn b)

instance ContourInstantiable FinalConstraint where
  instContours vs cn (FinalConstraint b) =
    FinalConstraint $ instContours vs cn b

instance ContourInstantiable ImmutableConstraint where
  instContours vs cn (ImmutableConstraint b) =
    ImmutableConstraint $ instContours vs cn b

instance ContourInstantiable FlowConstraint where
  instContours vs cn (FlowConstraint a k a') =
    FlowConstraint (instContours vs cn a) k (instContours vs cn a')

instance ContourInstantiable ExceptionConstraint where
  instContours vs cn (ExceptionConstraint a a') =
    ExceptionConstraint (instContours vs cn a) (instContours vs cn a') 
    
instance ContourInstantiable CellTVar where
  instContours vs cn b =
    let (con,PossibleContour mcn') =
          case b of
            CellTVar _y _pcn' -> (CellTVar _y,_pcn')
            GenCellTVar _x _pcn' -> (GenCellTVar _x,_pcn')
    in
    if SomeCellTVar b `Set.member` vs || isNothing mcn'
      then b
      else con $ PossibleContour $ Just cn

instance ContourInstantiable FlowTVar where
  instContours vs cn a =
    let (con,PossibleContour mcn') =
          case a of
            FlowTVar _x _pcn' -> (FlowTVar _x,_pcn')
            GenFlowTVar _x _pcn' -> (GenFlowTVar _x,_pcn')
    in
    if SomeFlowTVar a `Set.member` vs || isNothing mcn'
      then a
      else con $ PossibleContour $ Just cn

instance (ConstraintDatabase db) => ContourInstantiable (Type db) where
  instContours vs cn arg = case arg of
    Primitive _ -> arg
    EmptyOnion -> arg
    Label n b -> Label n $ instContours vs cn b
    Onion a a' -> Onion (instContours vs cn a) (instContours vs cn a')
    OnionFilter a op proj -> OnionFilter (instContours vs cn a) op proj
    Scape tpat a db -> Scape tpat a $ instantiateContours
      (vs `Set.union` Set.map SomeCellTVar (patVars tpat)
          `Set.union` boundVariables db) cn db
    where
      patVars :: PatternType -> Set CellTVar
      patVars tpat = case tpat of
        ValuePattern y tipat -> Set.insert y $ ipatVars tipat
        ExnPattern y tipat -> Set.insert y $ ipatVars tipat
      ipatVars :: InnerPatternType -> Set CellTVar
      ipatVars tipat = case tipat of
        PrimitivePattern _ -> Set.empty
        LabelPattern _ y ipat -> Set.insert y $ ipatVars ipat
        ConjunctivePattern ipat ipat' -> ipatVars ipat `Set.union`
                                         ipatVars ipat'
        ScapePattern -> Set.empty
        EmptyOnionPattern -> Set.empty

instance ContourInstantiable PatternType where
  instContours vs cn arg = case arg of
    ValuePattern y ipat ->
      ValuePattern (instContours vs cn y) (instContours vs cn ipat)
    ExnPattern y ipat ->
      ExnPattern (instContours vs cn y) (instContours vs cn ipat)

instance ContourInstantiable InnerPatternType where
  instContours vs cn arg = case arg of
    PrimitivePattern _ -> arg
    LabelPattern n y ipat ->
      LabelPattern n (instContours vs cn y) (instContours vs cn ipat)
    ConjunctivePattern ipat ipat' ->
      ConjunctivePattern (instContours vs cn ipat) (instContours vs cn ipat')
    ScapePattern -> arg
    EmptyOnionPattern -> arg
    
-- Instances for history

instance (ConstraintDatabase db, ContourInstantiable db)
      => ContourInstantiable (ConstraintHistory db) where
  instContours vs cn arg = case arg of
    DerivedFromSource _ -> arg
    CompatibilityWiring -> arg
    DerivedFromClosure rule -> DerivedFromClosure $ instContours vs cn rule
    
instance (ConstraintDatabase db, ContourInstantiable db)
      => ContourInstantiable (ClosureRule db) where
  instContours vs cn arg = case arg of
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
      rec :: (ContourInstantiable a) => a -> a
      rec = instContours vs cn

instance (ConstraintDatabase db)
      => ContourInstantiable (SingleProjectionResult db) where
  instContours vs cn (SingleProjectionResult proj a mt fib) =
    SingleProjectionResult proj (instContours vs cn a)
        (instContours vs cn mt) (instContours vs cn fib)

instance (ConstraintDatabase db)
      => ContourInstantiable (MultiProjectionResult db) where
  instContours vs cn (MultiProjectionResult proj a ts fib) =
    MultiProjectionResult proj (instContours vs cn a)
        (instContours vs cn ts) (instContours vs cn fib)
        
instance (ConstraintDatabase db, ContourInstantiable db)
      => ContourInstantiable (ApplicationCompatibilityResult db) where
  instContours vs cn (ApplicationCompatibilityResult ca ts mdat fib) =
    ApplicationCompatibilityResult (instContours vs cn ca)
      (instContours vs cn ts) (instContours vs cn mdat) (instContours vs cn fib)
      
instance ContourInstantiable CellLowerBoundingConstraint where
  instContours vs cn lbc = case lbc of
    CellCreationLowerBoundingConstraint c ->
      CellCreationLowerBoundingConstraint $ instContours vs cn c
    CellSettingLowerBoundingConstraint c ->
      CellSettingLowerBoundingConstraint $ instContours vs cn c
      
instance (ConstraintDatabase db)
      => ContourInstantiable (Fibration db) where
  instContours vs cn fib = case fib of
    Unexpanded -> Unexpanded
    Fibration t fs -> Fibration (instContours vs cn t) (instContours vs cn fs)
    
instance ContourInstantiable CompatibilityArgument where
  instContours vs cn ca = case ca of
    ArgVal a -> ArgVal $ instContours vs cn a
    ArgExn a -> ArgExn $ instContours vs cn a
