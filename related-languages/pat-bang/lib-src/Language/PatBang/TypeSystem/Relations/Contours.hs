{-# LANGUAGE FlexibleInstances, UndecidableInstances, TemplateHaskell #-}

module Language.PatBang.TypeSystem.Relations.Contours
( cNew
, module Language.PatBang.TypeSystem.Relations.Contours.ContourExtractable
, module Language.PatBang.TypeSystem.Relations.Contours.ContourInstantiable
, module Language.PatBang.TypeSystem.Relations.Contours.ContourReplacable
) where

import Prelude hiding (foldr)

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast (FlowVar)
import Language.PatBang.Display
import Language.PatBang.Logging
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.Contours
import Language.PatBang.TypeSystem.Relations.Contours.ContourExtractable
import Language.PatBang.TypeSystem.Relations.Contours.ContourInstantiable
import Language.PatBang.TypeSystem.Relations.Contours.ContourReplacable
import Language.PatBang.TypeSystem.Types

$(loggingFunctions)

-- |Defines contour creation as specified in the PatBang language document.
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

-- |Defines contour collapse as specified in the PatBang language document.
cCollapse :: Contour -> Contour
cCollapse cn =
  let answer = 
        let strands = unContour cn in
        contour $ Set.map strandCollapse strands
  in
  _debugI ("Contour collapse of " ++ display cn ++ " is " ++ display answer) $
    answer
  where
    strandCollapse :: ContourStrand -> ContourStrand
    strandCollapse (ContourStrand parts) = ContourStrand $ partsCollapse parts
    partsCollapse :: [ContourPart] -> [ContourPart]
    partsCollapse elements =
      case elements of
        [] -> []
        el:els ->
          let indices = List.findIndices
                (not . Set.null . Set.intersection (partElements el)) $
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

-- |Defines contour widening.  This operation is equivalent to that specified
--  in the PatBang specification but includes an optimization: if any contour
--  strand in the intersecting contours completely subsumes another, the latter
--  contour strand is discarded.
cWiden :: (ConstraintDatabase db) => Contour -> db -> Contour
cWiden cn c =
  let otherContours = filter (overlap cn) $ Set.toList $ getAllContours c in
  if null otherContours
    then cn
    else
      -- Begin by collecting all of the strands
      let strands = concatMap (Set.toList . unContour) $ cn:otherContours in
      -- Now chew through them in order and eliminate the extras
      let strands' = strandReduction [] strands in
      -- And made a contour
      contour $ Set.fromList strands'
  where
    strandSubsumedBy :: ContourStrand -> ContourStrand -> Bool
    strandSubsumedBy a b =
      contour (Set.singleton a) `subsumedBy` contour (Set.singleton b)
    strandNotSubsumedByList :: ContourStrand -> [ContourStrand] -> Bool
    strandNotSubsumedByList s = all $ not . (s `strandSubsumedBy`)
    strandReduction :: [ContourStrand] -> [ContourStrand] -> [ContourStrand]
    strandReduction knownGood toTest = case toTest of
      [] -> knownGood
      next:toTest' ->
        let notSubsumedByKnownGoodStrand =
              next `strandNotSubsumedByList` knownGood
            notSubsumedByRemainingTestStrand =
              next `strandNotSubsumedByList` toTest'
            necessaryStrand = notSubsumedByKnownGoodStrand
              && notSubsumedByRemainingTestStrand
        in strandReduction
              (if necessaryStrand then next:knownGood else knownGood) toTest'
            
-- |Creates a new contour based on an existing type variable and constraint
--  database.
cNew :: (ConstraintDatabase db) => FlowTVar -> db -> Contour
cNew a = cWiden (cCollapse $ tMakeCntr a)
