{-# LANGUAGE FlexibleInstances, OverlappingInstances, GADTs #-}

-- See notes/TypeConstraint-Overlapping.txt for explanation of
-- OverlappingInstances 

module Language.PatBang.TypeSystem.Relations.Contours.ContourInstantiable
( ContourInstantiable(..)
) where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Contours
import Language.PatBang.TypeSystem.Fibrations
import Language.PatBang.TypeSystem.Types

-- | A typeclass for contour instantiation.
class ContourInstantiable a where
  instContours :: Set FlowTVar -> Contour -> a -> a

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

instance ContourInstantiable FlowTVar where
  instContours vs cn a =
    let (con,PossibleContour mcn') =
          case a of
            FlowTVar _x _pcn' -> (FlowTVar _x,_pcn')
            GenFlowTVar _x _pcn' -> (GenFlowTVar _x,_pcn')
    in
    if a `Set.member` vs || isJust mcn'
      then a
      else con $ PossibleContour $ Just cn

instance (ConstraintDatabase db) => ContourInstantiable (Type db) where
  instContours vs cn arg = case arg of
    Primitive _ -> arg
    EmptyOnion -> arg
    Label n b -> Label n $ instContours vs cn b
    Onion a a' -> Onion (instContours vs cn a) (instContours vs cn a')
    Function aa a db -> Function aa a $ instantiateContours
      (a `Set.insert` vs `Set.union` Set.fromList aa) cn db
    Pattern bs p -> Pattern bs $ instContours vs cn p
    Scape a a' -> Scape  (instContours vs cn a) (instContours vs cn a')

instance ContourInstantiable PatternBody where
  instContours vs cn arg = case arg of
    PPrim t -> PPrim t
    PLabel n tpat -> PLabel n $ instContours vs cn tpat
    PFun -> PFun
    PPat -> PPat
    PScape -> PScape
    PConj tpat tpat' ->
      PConj (instContours vs cn tpat) (instContours vs cn tpat')
    PDisj tpat tpat' ->
      PDisj (instContours vs cn tpat) (instContours vs cn tpat')
    PSubst a tpats ->
      PSubst (instContours vs cn a) (map (instContours vs cn) tpats)
    PPatternOf a ->
      PPatternOf $ instContours vs cn a
    PRec bs tpat -> PRec bs $ instContours vs cn tpat
    PVar b -> PVar b
    PNone -> PNone

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
    where
      rec :: (ContourInstantiable a) => a -> a
      rec = instContours vs cn

instance (ConstraintDatabase db)
      => ContourInstantiable (ProjectionResult db) where
  instContours vs cn (ProjectionResult proj a form) =
    ProjectionResult proj (instContours vs cn a) (instContours vs cn form)

instance (ConstraintDatabase db)
      => ContourInstantiable (ProjectionResultForm db tag) where
  instContours vs cn form = case form of
    ProjectionResultPrimForm x f ->
      ProjectionResultPrimForm x (instContours vs cn f)
    ProjectionResultLabelForm bs f ->
      ProjectionResultLabelForm (instContours vs cn bs) (instContours vs cn f)
    ProjectionResultFunForm ts f ->
      ProjectionResultFunForm (instContours vs cn ts) (instContours vs cn f)
    ProjectionResultPatForm ts f ->
      ProjectionResultPatForm (instContours vs cn ts) (instContours vs cn f)
    ProjectionResultScapeForm ts f ->
      ProjectionResultScapeForm (instContours vs cn ts) (instContours vs cn f)
        
instance (ConstraintDatabase db, ContourInstantiable db)
      => ContourInstantiable (ApplicationCompatibilityResult db) where
  instContours vs cn (ApplicationCompatibilityResult ca ts mdat fib) =
    ApplicationCompatibilityResult (instContours vs cn ca)
      (instContours vs cn ts) (instContours vs cn mdat) (instContours vs cn fib)
      
instance (ConstraintDatabase db)
      => ContourInstantiable (Fibration db) where
  instContours vs cn fib = case fib of
    Unexpanded -> Unexpanded
    Fibration t fs -> Fibration (instContours vs cn t) (instContours vs cn fs)
