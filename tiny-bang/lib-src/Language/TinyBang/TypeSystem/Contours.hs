{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TemplateHaskell #-}

{-|
  This module defines polymorphic contours in TinyBang.
-}
module Language.TinyBang.TypeSystem.Contours
( Contour

, PossibleContour(..)
, unPossibleContour

, initialContour
, noContour

, subsumedBy
, overlap
, extend
, union
) where

import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger
import qualified Language.TinyBang.Utils.Data.NFA as NFA

$(loggingFunctions)

-- * Contour structures

newtype ContourElement = ContourElement Var
  deriving (Eq, Ord, Show)

data ContourPart
  = SinglePart ContourElement
  | SetPart (Set ContourElement)
  deriving (Eq, Ord, Show)
  
data ContourStrand =
  ContourStrand
    { contourParts :: [ContourPart]
    , appearingElements :: Set ContourElement
    }
  deriving (Eq, Ord, Show)

data Contour =
  Contour
    { contourStrands :: Set ContourStrand
    , contourNfa :: NFA.Nfa ContourElement
    }

instance Eq Contour where
  (==) = (==) `on` contourStrands
instance Ord Contour where
  compare = compare `on` contourStrands
instance Show Contour where
  show = show . contourStrands
  
newtype PossibleContour = PossibleContour (Maybe Contour)
  deriving (Eq, Ord, Show)
  
unPossibleContour :: PossibleContour -> Maybe Contour
unPossibleContour (PossibleContour mcntr) = mcntr

-- * Contour contents

-- | The initial contour.
initialContour :: Contour
initialContour =
  Contour
    { contourStrands = Set.empty
    , contourNfa = NFA.empty
    }

-- | The non-contour.
noContour :: PossibleContour
noContour = PossibleContour Nothing

-- * Contour operations

-- | Defines contour subsumption as specified in the TinyBang language document.
subsumedBy :: Contour -> Contour -> Bool
subsumedBy cn1 cn2 =
  let answer = 
        NFA.isEmpty $ NFA.subtract (contourNfa cn1) (contourNfa cn2)
      message = "Contour subsumption check: " ++ display cn2 ++
        (if answer then " subsumes " else " does not subsume ") ++ display cn1
  in
  _debugI message answer

-- | Defines contour overlap as specified in the TinyBang language document.
overlap :: Contour -> Contour -> Bool
overlap cn1 cn2 =
  not $ NFA.isEmpty $ NFA.intersect (contourNfa cn1) (contourNfa cn2)
  
-- | Extends a contour with a single variable element.  If this causes the
--   contour to become ill-formed, it is then folded into the least well-formed
--   contour.
extend :: Var -> Contour -> Contour
extend x cntr =
  let newStrands = Set.map extendStrand $ contourStrands cntr in
  if any (elmt `Set.member`) $ map appearingElements $ Set.toList $
        contourStrands cntr
    then
      Contour
        { contourStrands = newStrands
        , contourNfa = nfaFromContourStrands newStrands
        }
    else
      Contour
        { contourStrands = newStrands
        , contourNfa = NFA.addSuffix elmt $ contourNfa cntr
        }
  where
    elmt = ContourElement x
    extendStrand :: ContourStrand -> ContourStrand
    extendStrand strand =
      if elmt `Set.member` appearingElements strand
        then
          ContourStrand
            { contourParts =
                reverse $ collapse $ reverse $ contourParts strand
            , appearingElements = appearingElements strand
            }
        else
          ContourStrand
            { contourParts = (++ [SinglePart elmt]) $ contourParts strand
            , appearingElements = Set.insert elmt $ appearingElements strand
            }
    collapse :: [ContourPart] -> [ContourPart]
    collapse = f Set.empty
      where
        f vars parts =
          case parts of
            [] -> [SetPart vars]
            part:parts' ->
              if elmt `Set.member` elmtsOf part
                then SetPart (vars `Set.union` elmtsOf part):parts'
                else f (vars `Set.union` elmtsOf part) parts'
          where
            elmtsOf (SinglePart e) = Set.singleton e
            elmtsOf (SetPart es) = es
    nfaFromContourStrands :: Set ContourStrand -> NFA.Nfa ContourElement
    nfaFromContourStrands strands =
      let nfas = map nfaFromContourStrand $ Set.toList strands in
      if null nfas then NFA.empty else foldl1 NFA.union nfas
      where
        nfaFromContourStrand :: ContourStrand -> NFA.Nfa ContourElement
        nfaFromContourStrand strand =
          let nfas = map nfaFromContourPart $ contourParts strand in
          if null nfas then NFA.emptyString else foldl1 NFA.concatenate nfas
          where
            nfaFromContourPart :: ContourPart -> NFA.Nfa ContourElement
            nfaFromContourPart part = case part of
              SinglePart e -> NFA.singleton e
              SetPart es -> NFA.kleeneSingleton $ Set.toList es
  
-- | Unions a contour with another contour.
union :: Contour -> Contour -> Contour
union cntr1 cntr2 =
  Contour
    { contourStrands = contourStrands cntr1 `Set.union` contourStrands cntr2
    , contourNfa = contourNfa cntr1 `NFA.union` contourNfa cntr2
    }

-- * Display instances

instance Display PossibleContour where
  makeDoc pc = case pc of
    PossibleContour Nothing -> char '*'
    PossibleContour (Just cn) -> makeDoc cn
    
instance Display Contour where
  makeDoc cn =
    let strands = contourStrands cn in
    if Set.size strands == 1
      then let [strand] = Set.toList strands in makeDoc strand
      else makeDoc strands

instance Display ContourStrand where
  makeDoc (ContourStrand parts _) = makeDoc parts
  
instance Display ContourPart where
  makeDoc part = case part of
    SinglePart e -> makeDoc e
    SetPart es -> makeDoc es

instance Display ContourElement where
  makeDoc (ContourElement x) = makeDoc x
