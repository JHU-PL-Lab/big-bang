{-# LANGUAGE LambdaCase #-}

{-|
  This module carries an implementation of the constraint database which uses
  a simple backing set of constraints.  Lookups and similar operations are
  O(n) time; no smart data structures are used.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple
( SimpleConstraintDatabase
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Relations
import Language.TinyBang.TypeSystem.Types

data SimpleConstraintDatabase
  = SimpleConstraintDatabase
      (Set (Constraint SimpleConstraintDatabase))
      (Map
        (Constraint SimpleConstraintDatabase)
        (ConstraintHistory SimpleConstraintDatabase))
  deriving (Eq, Ord, Show)

instance ConstraintDatabase SimpleConstraintDatabase where
  empty = SimpleConstraintDatabase Set.empty Map.empty
  add c hist (SimpleConstraintDatabase cs hists) =
    SimpleConstraintDatabase (Set.insert c cs) (Map.insert c hist hists)
  union (SimpleConstraintDatabase cs hists)
        (SimpleConstraintDatabase cs' hists') =
    SimpleConstraintDatabase (cs `Set.union` cs') (hists `Map.union` hists')
    
  getAllConstraints (SimpleConstraintDatabase cs _) = cs
  getTypeConstraints = cfilter $ \case
      { WrapTypeConstraint c -> Just c; _ -> Nothing }
  getIntegerCalculationConstraints = opfilter $ \case
      { OpPlus _ -> True; OpMinus _ -> True; _ -> False }
  getIntegerOperationConstraints = opfilter $ \case
      { OpGreater _ -> True; OpLess _ -> True; _ -> False }
  getEqualityConstraints = opfilter $ \case
      { OpEqual _ -> True; _ -> False }
  getCellLoadingConstraints = cfilter $ \case
      { WrapCellLoadingConstraint c -> Just c; _ -> Nothing }
  getExceptionConstraints = cfilter $ \case
      { WrapExceptionConstraint c -> Just c; _ -> Nothing }
  getApplicationConstraints = cfilter $ \case
      { WrapApplicationConstraint c -> Just c; _ -> Nothing }
  
  getTypeConstraintsByUpperBound a = cfilter $ \case
      WrapTypeConstraint tc@(TypeConstraint _ a') | a == a' -> Just tc
      _ -> Nothing
  getIntermediateConstraintsByLowerBound a = cfilter $ \case
      WrapIntermediateConstraint ic@(IntermediateConstraint a' _)
        | a == a' -> Just ic
      _ -> Nothing
  getCellLowerBoundConstraints b = cfilter $ \case
      WrapCellCreationConstraint c@(CellCreationConstraint _ b') | b == b' ->
        Just $ CellCreationLowerBoundingConstraint c
      WrapCellSettingConstraint c@(CellSettingConstraint _ b') | b == b' ->
        Just $ CellSettingLowerBoundingConstraint c
      _ -> Nothing
  getFlowConstraintsByLowerBound a k = cfilter $ \case
      WrapFlowConstraint c@(FlowConstraint a' k' _) | a == a' && k == k' ->
        Just c
      _ -> Nothing
  getExceptionConstraintsByUpperBound a = cfilter $ \case
      WrapExceptionConstraint c@(ExceptionConstraint _ a') | a == a' -> Just c
      _ -> Nothing

  getAllContours (SimpleConstraintDatabase cs _) = extractContours cs

cfilter :: (Ord a, ConstraintDatabase db)
        => (Constraint db -> Maybe a) -> db -> [a]
cfilter f = mapMaybe f . Set.toList . getAllConstraints

opfilter :: (ConstraintDatabase db)
         => (BinaryOperator -> Bool) -> db -> [OperationConstraint]
opfilter f = cfilter $ \case
    WrapOperationConstraint (oc@(OperationConstraint _ op _ _)) ->
      if f op then Just oc else Nothing
    _ -> Nothing
