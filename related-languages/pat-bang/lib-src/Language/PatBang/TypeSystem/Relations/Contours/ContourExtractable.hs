{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

module Language.PatBang.TypeSystem.Relations.Contours.ContourExtractable
( ContourExtractable(..)
) where

-- See notes/TypeConstraint-Overlapping.txt for explanation of
-- OverlappingInstances 

import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.Contours
import Language.PatBang.TypeSystem.Types

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

instance ContourExtractable FlowTVar where
  extractContours a = case a of
    FlowTVar _ cn -> extractContours cn
    GenFlowTVar _ cn -> extractContours cn
    
instance ContourExtractable PatTVar where
  extractContours _ = Set.empty

instance (ConstraintDatabase db) => ContourExtractable (Type db) where
  extractContours arg = case arg of
    Primitive _ -> Set.empty
    EmptyOnion -> Set.empty
    Label _ a -> extractContours a
    Onion a a' -> extractContours a `Set.union` extractContours a'
    Function aa a db -> extractContours aa `Set.union` extractContours a
                        `Set.union` getAllContours db
    Pattern _ p -> extractContours p
    Scape a a' -> extractContours a `Set.union` extractContours a'
    
instance ContourExtractable PatternBody where
  extractContours arg = case arg of
    PPrim _ -> Set.empty
    PLabel _ tpat -> extractContours tpat
    PFun -> Set.empty
    PPat -> Set.empty
    PScape -> Set.empty
    PConj tpat tpat' -> extractContours tpat `Set.union` extractContours tpat'
    PSubst a tpats -> Set.unions (extractContours a:map extractContours tpats)
    PRec _ tpat -> extractContours tpat
    PVar _ -> Set.empty
    
instance ContourExtractable PossibleContour where
  extractContours (PossibleContour mcn) = extractContours mcn

instance ContourExtractable Contour where
  extractContours = Set.singleton
