{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

module Language.TinyBang.TypeSystem.Relations.Contours.ContourExtractable
( ContourExtractable(..)
) where

-- See notes/TypeConstraint-Overlapping.txt for explanation of
-- OverlappingInstances 

import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types

-- | A typeclass for contour extraction.
class ContourExtractable a where
  extractContours :: a -> Set Contour

instance (ContourExtractable a, Data.Foldable.Foldable t)
      => ContourExtractable (t a) where
  extractContours = Data.Foldable.foldMap extractContours
  
instance (ConstraintDatabase db) => ContourExtractable (Constraint db) where
  extractContours arg = case arg of
    WrapIntermediateConstraint c -> extractContours c
    WrapTypeConstraint c -> extractContours c
    WrapApplicationConstraint c -> extractContours c
    WrapOperationConstraint c -> extractContours c
    WrapCellCreationConstraint c -> extractContours c
    WrapCellLoadingConstraint c -> extractContours c
    WrapCellSettingConstraint c -> extractContours c
    WrapFinalConstraint c -> extractContours c
    WrapImmutableConstraint c -> extractContours c
    WrapFlowConstraint c -> extractContours c
    WrapExceptionConstraint c -> extractContours c

instance ContourExtractable IntermediateConstraint where
  extractContours (IntermediateConstraint a a') =
    extractContours a `Set.union` extractContours a'
  
instance (ConstraintDatabase db) => ContourExtractable (TypeConstraint db) where
  extractContours (TypeConstraint t a) =
    extractContours t `Set.union` extractContours a

instance ContourExtractable ApplicationConstraint where
  extractContours (ApplicationConstraint a a' a'') = 
    extractContours a `Set.union` extractContours a'
      `Set.union` extractContours a''

instance ContourExtractable OperationConstraint where
  extractContours (OperationConstraint a _ a' a'') = 
    extractContours a `Set.union` extractContours a'
      `Set.union` extractContours a''

instance ContourExtractable CellCreationConstraint where
  extractContours (CellCreationConstraint a b) =
    extractContours a `Set.union` extractContours b

instance ContourExtractable CellLoadingConstraint where
  extractContours (CellLoadingConstraint b a) = 
    extractContours b `Set.union` extractContours a

instance ContourExtractable CellSettingConstraint where
  extractContours (CellSettingConstraint a b) = 
    extractContours a `Set.union` extractContours b

instance ContourExtractable FinalConstraint where
  extractContours (FinalConstraint b) = extractContours b

instance ContourExtractable ImmutableConstraint where
  extractContours (ImmutableConstraint b) = extractContours b 

instance ContourExtractable FlowConstraint where
  extractContours (FlowConstraint a _ a') =
    extractContours a `Set.union` extractContours a'

instance ContourExtractable ExceptionConstraint where
  extractContours (ExceptionConstraint a a') = 
    extractContours a `Set.union` extractContours a'
    
instance ContourExtractable CellTVar where
  extractContours b = case b of
    CellTVar _ cn -> extractContours cn
    GenCellTVar _ cn -> extractContours cn

instance ContourExtractable FlowTVar where
  extractContours a = case a of
    FlowTVar _ cn -> extractContours cn
    GenFlowTVar _ cn -> extractContours cn

instance (ConstraintDatabase db) => ContourExtractable (Type db) where
  extractContours arg = case arg of
    Primitive _ -> Set.empty
    EmptyOnion -> Set.empty
    Label _ b -> extractContours b
    Onion a a' -> extractContours a `Set.union` extractContours a'
    OnionFilter a _ _ -> extractContours a
    Scape pat a db -> extractContours pat `Set.union` extractContours a
                        `Set.union` getAllContours db

instance ContourExtractable PatternType where
  extractContours arg = case arg of
    ValuePattern y ipat -> extractContours y `Set.union` extractContours ipat
    ExnPattern y ipat -> extractContours y `Set.union` extractContours ipat

instance ContourExtractable InnerPatternType where
  extractContours arg = case arg of
    PrimitivePattern _ -> Set.empty
    LabelPattern _ y ipat -> extractContours y `Set.union` extractContours ipat
    ConjunctivePattern ipat ipat' ->
      extractContours ipat `Set.union` extractContours ipat'
    ScapePattern -> Set.empty
    EmptyOnionPattern -> Set.empty
    
instance ContourExtractable PossibleContour where
  extractContours (PossibleContour mcn) = extractContours mcn

instance ContourExtractable Contour where
  extractContours = Set.singleton
