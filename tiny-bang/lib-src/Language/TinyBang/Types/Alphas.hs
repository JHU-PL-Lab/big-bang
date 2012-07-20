{-# LANGUAGE Rank2Types,
             ExistentialQuantification,
             EmptyDataDecls,
             FlexibleInstances #-}
module Language.TinyBang.Types.Alphas
( AlphaId
, Alpha(..)
, InterAlpha
, InterType
, CellAlpha
, CellType
, AnyAlpha
, SomeAlpha
, AlphaType
, ProgramLabel
, CallSite
, Contour
, contour
, unContour
, alpha
, makeNewAlpha
) where

import Data.Adj
import Utils.Render.Display

-- TODO: make ProgramLabel more precise
type ProgramLabel = Integer

-- |The thing we use to identify and distinguish alphas
type AlphaId = ProgramLabel

data SomeAlpha alphaType = SomeAlpha AlphaId Contour
  deriving (Eq, Ord, Show)

alpha :: (AlphaType a) => AlphaId -> Contour -> SomeAlpha a
alpha = SomeAlpha

makeNewAlpha :: (AlphaType a) => AlphaId -> SomeAlpha a
makeNewAlpha i = alpha i $ contour $ []

data InterType
data CellType

class AlphaType a
instance AlphaType InterType
instance AlphaType CellType

class Alpha a where
  alphaId :: a -> AlphaId
  alphaContour :: a -> Contour
  -- setAlphaId :: a -> AlphaId -> a
  setAlphaContour :: a -> Contour -> a
  alphaWeaken :: a -> AnyAlpha

instance (AlphaType a) => Alpha (SomeAlpha a) where
  alphaId (SomeAlpha i _) = i
  alphaContour (SomeAlpha _ c) = c
  -- setAlphaId a aid = alpha aid (alphaCallSites a)
  setAlphaContour a css = alpha (alphaId a) css
  alphaWeaken = AnyAlpha

instance Alpha AnyAlpha where
  alphaId (AnyAlpha a) = alphaId a
  alphaContour (AnyAlpha a) = alphaContour a
  setAlphaContour a css = setAlphaContour a css
  alphaWeaken = id

--insertIntoCallSites :: CellAlpha -> AnyAlpha -> AnyAlpha

type InterAlpha = SomeAlpha InterType
type CellAlpha = SomeAlpha CellType
data AnyAlpha = forall a. (AlphaType a) => AnyAlpha (SomeAlpha a)

instance Eq AnyAlpha where
  AnyAlpha (SomeAlpha i c) == AnyAlpha (SomeAlpha i' c') =
    (i, c) == (i', c')

instance Ord AnyAlpha where
  AnyAlpha (SomeAlpha i c) `compare` AnyAlpha (SomeAlpha i' c') =
    (i, c) `compare` (i', c')

instance Adj AnyAlpha where
  (AnyAlpha (SomeAlpha i c)) `adjacent` (AnyAlpha (SomeAlpha i' c')) =
    (c == c') && (i `adjacent` i')

instance Show AnyAlpha where
  show (AnyAlpha a) = show a

type CallSite = ProgramLabel

-- |A data structure representing a type contour.  Type contours are described
--  in terms of a sequence of call sites.  In the non-recursive case, this
--  sequence exactly matches the call sequence used to reach a given point in
--  the program; for recursive cases, contours are reused.  In this
--  implementation, the call site list is reversed; that is, the head of the
--  call site list is the *most recent* call site that has been visited.
newtype Contour = Contour { unContour :: [CallSite] }
    deriving (Eq, Ord, Show)
contour :: [CallSite] -> Contour
contour = Contour


instance Display (SomeAlpha a) where
  makeDoc (SomeAlpha i cntr) =
      char '\'' <> makeDoc i <> (
          if not . null $ unContour cntr
              then char '^' <> makeDoc cntr
              else empty)

instance Display AnyAlpha where
  makeDoc (AnyAlpha a) = makeDoc a

instance Display Contour where
  makeDoc (Contour c) = makeDoc c
