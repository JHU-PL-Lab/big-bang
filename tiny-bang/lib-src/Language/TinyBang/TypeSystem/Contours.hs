{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}

module Language.TinyBang.TypeSystem.Contours
( ContourElement(..)
, ContourPart(..)
, ContourStrand(..)
, Contour
, contour
, unContour
, PossibleContour(..)
, subsumedBy
, overlap
, initialContour
, noContour
) where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.NFA as NFA
import Language.TinyBang.Ast
import Language.TinyBang.Display

newtype ContourElement = ContourElement FlowVar
  deriving (Eq, Ord, Show)

data ContourPart
  = SinglePart ContourElement
  | SetPart (Set ContourElement)
  deriving (Eq, Ord, Show)
  
newtype ContourStrand = ContourStrand [ContourPart]
  deriving (Eq, Ord, Show)

data Contour = Contour (Set ContourStrand) ContourNfa

newtype PossibleContour = PossibleContour (Maybe Contour)
  deriving (Eq, Ord, Show)

data ContourNfa
  = forall a. (Eq a, Ord a) => ContourNfa (NFA.Nfa a ContourElement)

instance Eq Contour where
  (Contour s _) == (Contour s' _) = s == s'
instance Ord Contour where
  (Contour s _) `compare` (Contour s' _) = s `compare` s'
instance Show Contour where
  show (Contour s _) = show s
  
-- | Defines contour subsumption as specified in the TinyBang language document.
subsumedBy :: Contour -> Contour -> Bool
subsumedBy (Contour _ (ContourNfa nfa1)) (Contour _ (ContourNfa nfa2)) =
  NFA.isEmpty $ NFA.subtract nfa1 nfa2

-- | Defines contour overlap as specified in the TinyBang language document.
overlap :: Contour -> Contour -> Bool
overlap (Contour _ (ContourNfa nfa1)) (Contour _ (ContourNfa nfa2)) =
  not $ NFA.isEmpty $ NFA.intersect nfa1 nfa2

-- | The initial contour.
initialContour :: Contour
initialContour = contour $ Set.singleton $ ContourStrand []

-- | The non-contour.
noContour :: PossibleContour
noContour = PossibleContour Nothing

-- | Creates a type contour from just a set of strands.  This function is the
--   normal constructor for type contours; it generates an NFA from the strands
--   provided.
contour :: Set ContourStrand -> Contour
contour strands = Contour strands $
    foldr (contourNfaUnion . nfaFromContourStrand) (ContourNfa NFA.empty)
      (Set.toList strands)
  where
    contourNfaUnion :: ContourNfa -> ContourNfa -> ContourNfa
    contourNfaUnion (ContourNfa nfa1) (ContourNfa nfa2) =
      ContourNfa $ NFA.union nfa1 nfa2

-- | Retrieves the strands from a type contour.
unContour :: Contour -> Set ContourStrand
unContour (Contour strands _) = strands

-- | Builds an NFA from a contour strand.
nfaFromContourStrand :: ContourStrand -> ContourNfa
nfaFromContourStrand (ContourStrand parts) =
  ContourNfa $ NFA.createFromDataWithEpsilon 0 edges [length edges]
  where
    edges :: [(Int,Maybe ContourElement,Int)]
    edges = concatMap edgesFromPart (zip [0 ..] parts)
    edgesFromPart :: (Int,ContourPart) -> [(Int, Maybe ContourElement, Int)]
    edgesFromPart (idx,part) =
      case part of
        SinglePart e -> [(idx,Just e,idx+1)]
        SetPart es ->
          (idx, Nothing, idx + 1) :
            map (\ e -> (idx, Just e, idx)) (Set.toList es)

instance Display PossibleContour where
  makeDoc pc = case pc of
    PossibleContour Nothing -> char '*'
    PossibleContour (Just cn) -> makeDoc cn
    
instance Display Contour where
  makeDoc cn = makeDoc $ unContour cn

instance Display ContourStrand where
  makeDoc (ContourStrand parts) = makeDoc parts
  
instance Display ContourPart where
  makeDoc part = case part of
    SinglePart e -> makeDoc e
    SetPart es -> makeDoc es

instance Display ContourElement where
  makeDoc (ContourElement x) = makeDoc x