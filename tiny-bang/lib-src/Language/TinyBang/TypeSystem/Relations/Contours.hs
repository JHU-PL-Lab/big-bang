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
import Data.Maybe
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast (FlowVar)
import Language.TinyBang.Display
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types

-- |Defines contour creation as specified in the TinyBang language document.
tMakeCntr :: FlowTVar -> Contour
tMakeCntr a =
  let (x,pcn) = case a of
                  FlowTVar x' pcn' -> (x',pcn')
                  GenFlowTVar _ _ ->
                    error $ "tMakeCntr called on generated variable "
                              ++ display a
  in
  case pcn of
    PossibleContour Nothing ->
      error $ "tMakeCntr called on non-contoured variable " ++ display a
    PossibleContour (Just cn) ->
      contour $ Set.map (extendStrand x) $ unContour cn
  where
    extendStrand :: FlowVar -> ContourStrand -> ContourStrand
    extendStrand x (ContourStrand es) =
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
  instContours :: Set AnyTVar -> Contour -> a -> a

-- | A typeclass for contour replacement.
class ContourReplacable a where
  replContours :: Contour -> a -> a

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

---- Typeclass instances for contour instantiation

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