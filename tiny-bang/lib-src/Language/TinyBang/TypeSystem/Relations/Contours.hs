{-# LANGUAGE FlexibleInstances, UndecidableInstances, TemplateHaskell #-}

module Language.TinyBang.TypeSystem.Relations.Contours
( cNew
, module Language.TinyBang.TypeSystem.Relations.Contours.ContourExtractable
, module Language.TinyBang.TypeSystem.Relations.Contours.ContourInstantiable
, module Language.TinyBang.TypeSystem.Relations.Contours.ContourReplacable
) where

import Prelude hiding (foldr)

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast (FlowVar)
import Language.TinyBang.Display
import Language.TinyBang.Logging
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Relations.Contours.ContourExtractable
import Language.TinyBang.TypeSystem.Relations.Contours.ContourInstantiable
import Language.TinyBang.TypeSystem.Relations.Contours.ContourReplacable
import Language.TinyBang.TypeSystem.Types

$(loggingFunctions)

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
