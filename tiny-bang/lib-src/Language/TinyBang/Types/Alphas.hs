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
, AlphaSubstitutionEnv
, ProgramLabel
, FunctionLowerBound
, CallSite
, Contour
, contour
, unContour
, alpha
, makeNewAlpha
) where

import Utils.Render.Display

import Data.Set (Set)

import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: make ProgramLabel more precise
type ProgramLabel = Integer

-- |The thing we use to identify and distinguish alphas
type AlphaId = ProgramLabel

data SomeAlpha alphaType = SomeAlpha AlphaId Contour
  deriving (Eq, Ord, Show)

alpha :: (AlphaType a) => AlphaId -> Contour -> SomeAlpha a
alpha = SomeAlpha

makeNewAlpha :: (AlphaType a) => AlphaId -> SomeAlpha a
makeNewAlpha i = alpha i $ contour $ Map.empty

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

instance Show AnyAlpha where
  show (AnyAlpha a) = show a

type FunctionLowerBound = ProgramLabel
type CallSite = ProgramLabel

-- TODO: Update this doc; it references call sites.
-- |A data structure representing function call sites.  The call site of a
--  function application is defined by the type parameter used as the domain
--  of the function type upper bounding the function in that application.  The
--  list of call sites is stored in reverse order; that is, the variable
--  '1^['2,'3] represents a1^{a3^{a2}}.  In the event of recursion, a set of
--  multiple call sites is grouped together.  For instance, the variable
--  '1^['3,'4,'3,'2] will be regrouped as the variable '1^[{'3,'4},'2].  Note
--  that, in this case, the use of single variables in the call site list is a
--  notational sugar for singleton sets.
newtype Contour = Contour { unContour :: Map FunctionLowerBound CallSite }
  deriving (Eq, Ord, Show)
contour :: Map FunctionLowerBound CallSite -> Contour
contour = Contour

-- TODO: Update this doc; it references call sites.
-- |This function transforms a specified alpha into a call site list.  The
--  resulting call site list is in the reverse order form dictated by the
--  CallSites structure; that is, the list [{'3},{'2},{'1}] represents the type
--  variable with the exponent expression ['1,'2,'3].  The resulting call site
--  list is suitable for use in type variable substitution for polymorphic
--  functions.  This function is used in closure.

type AlphaSubstitutionEnv = (Set AnyAlpha, CellAlpha, CellAlpha)

instance Display (SomeAlpha a) where
  makeDoc (SomeAlpha i cSites) =
      char '\'' <> makeDoc i <> (
          let doc = makeDoc cSites in
          if not $ isEmpty doc
              then char '^' <> doc
              else empty)

instance Display AnyAlpha where
  makeDoc (AnyAlpha a) = makeDoc a

instance Display Contour where
  makeDoc (Contour c) = makeDoc c
