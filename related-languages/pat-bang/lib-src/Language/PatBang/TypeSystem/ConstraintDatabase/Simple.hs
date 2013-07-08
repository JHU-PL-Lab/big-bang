{-# LANGUAGE LambdaCase #-}

{-|
  This module carries an implementation of the constraint database which uses
  a simple backing set of constraints.  Lookups and similar operations are
  O(n) time; no smart data structures are used.
-}
module Language.PatBang.TypeSystem.ConstraintDatabase.Simple
( SimpleConstraintDatabase
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast
import Language.PatBang.Display
import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Relations

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
  getApplicationConstraints = cfilter $ \case
      { WrapApplicationConstraint c -> Just c; _ -> Nothing }
  
  getTypeConstraintsByUpperBound a = cfilter $ \case
      WrapTypeConstraint tc@(TypeConstraint _ a') | a == a' -> Just tc
      _ -> Nothing
  getIntermediateConstraintsByLowerBound a = cfilter $ \case
      WrapIntermediateConstraint ic@(IntermediateConstraint a' _)
        | a == a' -> Just ic
      _ -> Nothing
      
  boundVariables (SimpleConstraintDatabase cs _) =
    Set.fromList $ mapMaybe boundVariableOf $ Set.toList cs
    where
      boundVariableOf = \case
        WrapIntermediateConstraint (IntermediateConstraint _ a) ->
          Just a
        WrapTypeConstraint (TypeConstraint _ a) -> Just a
        WrapApplicationConstraint (ApplicationConstraint _ _ a) ->
          Just a
        WrapOperationConstraint (OperationConstraint _ _ _ a) ->
          Just a

  getAllContours (SimpleConstraintDatabase cs _) = extractContours cs
  instantiateContours vs cn (SimpleConstraintDatabase cs hist) =
    SimpleConstraintDatabase (instContours vs cn cs) (instContours vs cn hist)
  replaceContours cn (SimpleConstraintDatabase cs hist) =
    SimpleConstraintDatabase (replContours cn cs) (replContours cn hist)

cfilter :: (Ord a, ConstraintDatabase db)
        => (Constraint db -> Maybe a) -> db -> [a]
cfilter f = mapMaybe f . Set.toList . getAllConstraints

opfilter :: (ConstraintDatabase db)
         => (BinaryOperator -> Bool) -> db -> [OperationConstraint]
opfilter f = cfilter $ \case
    WrapOperationConstraint (oc@(OperationConstraint _ op _ _)) ->
      if f op then Just oc else Nothing
    _ -> Nothing

instance ContourInstantiable SimpleConstraintDatabase where
  instContours = instantiateContours
instance ContourReplacable SimpleConstraintDatabase where
  replContours = replaceContours

instance Display SimpleConstraintDatabase where
  makeDoc (SimpleConstraintDatabase cs _) = makeDoc cs
