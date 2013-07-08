{-# LANGUAGE FlexibleInstances, OverlappingInstances, GADTs #-}

-- See notes/TypeConstraint-Overlapping.txt for explanation of
-- OverlappingInstances 

module Language.PatBang.TypeSystem.Relations.Contours.ContourReplacable
( ContourReplacable(..)
) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Contours
import Language.PatBang.TypeSystem.Fibrations
import Language.PatBang.TypeSystem.Types

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

instance (ConstraintDatabase db) => ContourReplacable (Type db) where
  replContours cn t = case t of
    Primitive _ -> t
    EmptyOnion -> t
    Label n b -> Label n $ replContours cn b
    Onion a a' -> Onion (replContours cn a) (replContours cn a')
    Function aa a cs ->
      Function (replContours cn aa) (replContours cn a) (replaceContours cn cs)
    Pattern bs p -> Pattern (replContours cn bs) (replContours cn p)
    Scape a a' -> Scape (replContours cn a) (replContours cn a')
      
instance ContourReplacable PatternBody where
  replContours cn tpat = case tpat of
    PPrim t -> PPrim t
    PLabel n tpat' -> PLabel n $ replContours cn tpat'
    PFun -> PFun
    PPat -> PPat
    PScape -> PScape
    PConj tpat' tpat'' -> PConj (replContours cn tpat') (replContours cn tpat'')
    PSubst a tpats -> PSubst (replContours cn a) (map (replContours cn) tpats)
    PRec bs tpat' -> PRec bs $ replContours cn tpat'
    PVar b -> PVar b

instance ContourReplacable FlowTVar where
  replContours cn a = case a of
    FlowTVar x pcn -> FlowTVar x $ replContours cn pcn
    GenFlowTVar x pcn -> GenFlowTVar x $ replContours cn pcn

instance ContourReplacable PatTVar where
  replContours _ b = b

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
    ProjectionResultPatForm ts f ->
      ProjectionResultPatForm (replContours cn ts) (replContours cn f)
    ProjectionResultScapeForm ts f ->
      ProjectionResultScapeForm (replContours cn ts) (replContours cn f)
        
instance (ConstraintDatabase db, ContourReplacable db)
      => ContourReplacable (ApplicationCompatibilityResult db) where
  replContours cn (ApplicationCompatibilityResult ca ts mdat fib) =
    ApplicationCompatibilityResult (replContours cn ca)
      (replContours cn ts) (replContours cn mdat) (replContours cn fib)
      
instance (ConstraintDatabase db)
      => ContourReplacable (Fibration db) where
  replContours cn fib = case fib of
    Unexpanded -> Unexpanded
    Fibration t fs -> Fibration (replContours cn t) (replContours cn fs)
