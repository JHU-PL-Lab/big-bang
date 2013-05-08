{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

-- See notes/TypeConstraint-Foldable.txt for explanation of OverlappingInstances 

module Language.TinyBang.TypeSystem.Relations.Contours
( cNew
, ContourExtractable(..)
, ContourInstantiable(..)
, ContourReplacable(..)
) where

import Prelude hiding (foldr)

import qualified Data.Foldable
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types

-- |Defines contour creation as specified in the TinyBang language document.
tMakeCntr :: FlowTVar -> Contour
tMakeCntr (FlowTVar x pcn) =
  case pcn of
    Nothing -> error $ "tMakeCntr called on non-contoured variable at "
                        ++ show x -- TODO: perhaps use Display?
    Just cn -> contour $ Set.map extendStrand $ unContour cn
  where
    extendStrand :: ContourStrand -> ContourStrand
    extendStrand (ContourStrand es) =
      ContourStrand $ es ++ [SinglePart $ ContourElement x]

-- |Defines contour collapse as specified in the TinyBang language document.
cCollapse :: Contour -> Contour
cCollapse cn =
  let strands = unContour cn in
  contour $ Set.map strandCollapse strands
  where
    strandCollapse :: ContourStrand -> ContourStrand
    strandCollapse (ContourStrand parts) = ContourStrand $ partsCollapse parts
    partsCollapse :: [ContourPart] -> [ContourPart]
    partsCollapse elements =
      case elements of
        [] -> []
        el:els ->
          let indices = List.findIndices
                (Set.null . Set.intersection (partElements el)) $
                map partElements els
          in
          if null indices
            then el : partsCollapse els
            else
              let (joins,rest) = List.splitAt (last indices + 1) els
                  joined = Set.foldr (Set.union . partElements)
                              (partElements el) $ Set.fromList joins
              in
              SetPart joined : partsCollapse rest
    partElements :: ContourPart -> Set ContourElement
    partElements part = case part of
      SinglePart e -> Set.singleton e
      SetPart es -> es

-- |Defines contour widening as specified in the TinyBang language document.
cWiden :: (ConstraintDatabase db) => Contour -> db -> Contour
cWiden cn c =
  let otherContours = filter (overlap cn) $ Set.toList $ getAllContours c in
  if null otherContours
    then cn
    else contour $ Set.foldr (Set.union . unContour) Set.empty $
            Set.fromList $ cn : otherContours
            
-- |Creates a new contour based on an existing type variable and constraint
--  database.
cNew :: (ConstraintDatabase db) => FlowTVar -> db -> Contour
cNew a = cWiden (cCollapse $ tMakeCntr a)

-- | A typeclass for contour extraction.
class ContourExtractable a where
  extractContours :: a -> Set Contour

-- | A typeclass for contour instantiation.
class ContourInstantiable a where
  instContours :: a -> Set FlowTVar -> Contour -> a

-- | A typeclass for contour replacement.
class ContourReplacable a where
  replContours :: a -> Contour -> a

-- Typeclass instances for contour extraction

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
  extractContours (CellTVar _ cn) = extractContours cn

instance ContourExtractable FlowTVar where
  extractContours (FlowTVar _ cn) = extractContours cn

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

instance ContourExtractable Contour where
  extractContours = Set.singleton

---- Typeclass instances for contour instantiation
--
--instance (Ord a, ContourInstantiable a) => ContourInstantiable (Set a) where
--  instContours t vs cn = Set.map (\x -> instContours x vs cn) t
--
--instance ContourInstantiable C.AnyConstraint where
--  instContours (C.AnyConstraint (C.AnyKindedShaped s k c)) vs cn =
--    C.AnyConstraint (C.AnyKindedShaped s k $ instContours c vs cn)
--
--instance (SingI k) => ContourInstantiable (C.Constraint s k) where
--  instContours c vs cn =
--    let rec :: (ContourInstantiable b) => b -> b
--        rec x = instContours x vs cn in
--    case c of
--      C.LowerBoundConstraint lb v ->
--        C.LowerBoundConstraint (rec lb) (rec v)
--      C.IntermediateConstraint v1 v2 ->
--        C.IntermediateConstraint (rec v1) (rec v2)
--      C.UpperBoundConstraint v ub ->
--        C.UpperBoundConstraint (rec v) (rec ub)
--      C.DirectConstraint lb ub ->
--        C.DirectConstraint (rec lb) (rec ub)
--      C.LazyOpConstraint v1 op v2 v3 ->
--        C.LazyOpConstraint (rec v1) op (rec v2) (rec v3)
--
--instance ContourInstantiable (T.LowerBound k) where
--  instContours t vs cn =
--    let rec :: (ContourInstantiable b) => b -> b
--        rec x = instContours x vs cn in
--    case t of
--      T.Prim p -> T.Prim p
--      T.Label n v -> T.Label n $ rec v
--      T.Onion v1 v2 -> T.Onion (rec v1) (rec v2)
--      T.Scape (T.ScapeData (T.ForallVars vs') pt v cs) ->
--        T.Scape (T.ScapeData (T.ForallVars $ rec vs') (rec pt) (rec v) (rec cs))
--      T.OnionSub v prj -> T.OnionSub (rec v) prj
--      T.OnionProj v prj -> T.OnionProj (rec v) prj
--      T.EmptyOnion -> T.EmptyOnion
--      T.Cell v -> T.Cell $ rec v
--
--instance ContourInstantiable (T.UpperBound k) where
--  instContours t vs cn =
--    let rec :: (ContourInstantiable b) => b -> b
--        rec x = instContours x vs cn in
--    case t of
--      T.Appl v1 v2 -> T.Appl (rec v1) (rec v2)
--      T.CellG v -> T.CellG $ rec v
--      T.CellS v -> T.CellS $ rec v
--
--instance ContourInstantiable P.AnyPatternType where
--  instContours (P.AnyPatternType p) vs cn =
--    P.AnyPatternType $ instContours p vs cn
--
--instance ContourInstantiable (P.PatternType tag) where
--  instContours (P.Pattern v ppt) vs cn =
--    let rec :: (ContourInstantiable b) => b -> b
--        rec x = instContours x vs cn in
--    P.Pattern (rec v) (rec ppt) 
--  
--instance ContourInstantiable P.PrimaryPatternType where
--  instContours ppt vs cn =
--    let rec :: (ContourInstantiable b) => b -> b
--        rec x = instContours x vs cn in
--    case ppt of
--      P.PatPrim p -> P.PatPrim p
--      P.PatLabel n v ppt' -> P.PatLabel n (rec v) (rec ppt')
--      P.PatOnion ppts -> P.PatOnion (map rec ppts)
--      P.PatFun -> P.PatFun
--
--instance ContourInstantiable V.AnyTypeVar where
--  instContours (K.AnyKinded k v) vs cn =
--    K.AnyKinded k $ instContours v vs cn
--  
--instance (SingI k) => ContourInstantiable (V.TypeVar k) where
--  instContours v@(V.TypeVar pl pc) vs cn =
--    if isNothing pc && V.anyTypeVar v `Set.member` vs
--      then V.TypeVar pl (Just cn)
--      else v
--
---- Typeclass instances for contour replacement
--
--instance (Ord a, ContourReplacable a) => ContourReplacable (Set a) where
--  replContours t cn = Set.map (`replContours` cn) t
--
--instance ContourReplacable C.AnyConstraint where
--  replContours (C.AnyConstraint (C.AnyKindedShaped s k c)) cn =
--    C.AnyConstraint (C.AnyKindedShaped s k $ replContours c cn)
--
--instance ContourReplacable (C.Constraint s k) where
--  replContours c cn =
--    let rec :: (ContourReplacable b) => b -> b
--        rec x = replContours x cn in
--    case c of
--      C.LowerBoundConstraint lb v ->
--        C.LowerBoundConstraint (rec lb) (rec v)
--      C.IntermediateConstraint v1 v2 ->
--        C.IntermediateConstraint (rec v1) (rec v2)
--      C.UpperBoundConstraint v ub ->
--        C.UpperBoundConstraint (rec v) (rec ub)
--      C.DirectConstraint lb ub ->
--        C.DirectConstraint (rec lb) (rec ub)
--      C.LazyOpConstraint v1 op v2 v3 ->
--        C.LazyOpConstraint (rec v1) op (rec v2) (rec v3)
--
--instance ContourReplacable (T.LowerBound k) where
--  replContours t cn =
--    let rec :: (ContourReplacable b) => b -> b
--        rec x = replContours x cn in
--    case t of
--      T.Prim p -> T.Prim p
--      T.Label n v -> T.Label n $ rec v
--      T.Onion v1 v2 -> T.Onion (rec v1) (rec v2)
--      T.Scape (T.ScapeData (T.ForallVars vs) pt v cs) ->
--        T.Scape (T.ScapeData (T.ForallVars $ rec vs) (rec pt) (rec v) (rec cs))
--      T.OnionSub v prj -> T.OnionSub (rec v) prj
--      T.OnionProj v prj -> T.OnionProj (rec v) prj
--      T.EmptyOnion -> T.EmptyOnion
--      T.Cell v -> T.Cell $ rec v
--
--instance ContourReplacable (T.UpperBound k) where
--  replContours t cn =
--    let rec :: (ContourReplacable b) => b -> b
--        rec x = replContours x cn in
--    case t of
--      T.Appl v1 v2 -> T.Appl (rec v1) (rec v2)
--      T.CellG v -> T.CellG $ rec v
--      T.CellS v -> T.CellS $ rec v
--
--instance ContourReplacable P.AnyPatternType where
--  replContours (P.AnyPatternType p) cn = P.AnyPatternType $ replContours p cn
--
--instance ContourReplacable (P.PatternType tag) where
--  replContours (P.Pattern v ppt) cn =
--    let rec :: (ContourReplacable b) => b -> b
--        rec x = replContours x cn in
--    P.Pattern (rec v) (rec ppt) 
--  
--instance ContourReplacable P.PrimaryPatternType where
--  replContours ppt cn =
--    let rec :: (ContourReplacable b) => b -> b
--        rec x = replContours x cn in
--    case ppt of
--      P.PatPrim p -> P.PatPrim p
--      P.PatLabel n v ppt' -> P.PatLabel n (rec v) (rec ppt')
--      P.PatOnion ppts -> P.PatOnion (map rec ppts)
--      P.PatFun -> P.PatFun
--
--instance ContourReplacable V.AnyTypeVar where
--  replContours (K.AnyKinded k v) cn =
--    let rec :: (ContourReplacable b) => b -> b
--        rec x = replContours x cn in
--    K.AnyKinded k $ rec v
--  
--instance ContourReplacable (V.TypeVar k) where
--  replContours (V.TypeVar pl pc) cn =
--    V.TypeVar pl $ replContours pc cn
--
--instance ContourReplacable (Maybe Contour) where
--  replContours pc cn =
--    fmap (flip replContours cn) pc
--
--instance ContourReplacable Contour where
--  replContours cn' cn =
--    if cn' `subsumedBy` cn then cn else cn'