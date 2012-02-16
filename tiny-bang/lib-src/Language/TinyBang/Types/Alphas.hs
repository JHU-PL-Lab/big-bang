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
, alpha
, makeNewAlpha
, substituteAlphaHelper
) where

import Utils.Render.Display

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Reader (Reader, ask)
import Control.Exception (assert)

-- |The thing we use to identify and distinguish alphas
type AlphaId = Integer

data SomeAlpha alphaType = SomeAlpha AlphaId CallSites
  deriving (Eq, Ord, Show)

alpha :: (AlphaType a) => AlphaId -> CallSites -> SomeAlpha a
alpha = SomeAlpha

makeNewAlpha :: (AlphaType a) => AlphaId -> SomeAlpha a
makeNewAlpha i = alpha i $ callSites []

data InterType
data CellType

class AlphaType a
instance AlphaType InterType
instance AlphaType CellType

class Alpha a where
  alphaId :: a -> AlphaId
  alphaCallSites :: a -> CallSites
  -- setAlphaId :: a -> AlphaId -> a
  setAlphaCallSites :: a -> CallSites -> a
  alphaWeaken :: a -> AnyAlpha

instance (AlphaType a) => Alpha (SomeAlpha a) where
  alphaId (SomeAlpha i _) = i
  alphaCallSites (SomeAlpha _ c) = c
  -- setAlphaId a aid = alpha aid (alphaCallSites a)
  setAlphaCallSites a css = alpha (alphaId a) css
  alphaWeaken = AnyAlpha

instance Alpha AnyAlpha where
  alphaId (AnyAlpha a) = alphaId a
  alphaCallSites (AnyAlpha a) = alphaCallSites a
  setAlphaCallSites a css = setAlphaCallSites a css
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

-- |A data structure representing a single call site.

-- @CallSite@ is a set of @CellAlpha@s because only variables on the input side
-- of a function arrow get added to them.
newtype CallSite = CallSite (Set CellAlpha)
  deriving (Eq, Ord, Show)

-- |A data structure representing function call sites.  The call site of a
--  function application is defined by the type parameter used as the domain
--  of the function type upper bounding the function in that application.  The
--  list of call sites is stored in reverse order; that is, the variable
--  '1^['2,'3] represents a1^{a3^{a2}}.  In the event of recursion, a set of
--  multiple call sites is grouped together.  For instance, the variable
--  '1^['3,'4,'3,'2] will be regrouped as the variable '1^[{'3,'4},'2].  Note
--  that, in this case, the use of single variables in the call site list is a
--  notational sugar for singleton sets.
newtype CallSites = CallSites { unCallSites :: [CallSite] }
  deriving (Eq, Ord, Show)
callSites :: [CallSite] -> CallSites
callSites lst = CallSites lst

-- |This function transforms a specified alpha into a call site list.  The
--  resulting call site list is in the reverse order form dictated by the
--  CallSites structure; that is, the list [{'3},{'2},{'1}] represents the type
--  variable with the exponent expression '1^('2^'3).  The resulting call site
--  list is suitable for use in type variable substitution for polymorphic
--  functions.  This function is used in closure.

-- This function takes @CellAlpha@s because @CallSites@ contains them.
makeCallSites :: CellAlpha -> CallSites
makeCallSites a =
    callSites $
    case rest of
      [] -> -- In this case, this call site is new to the list
        (CallSite $ Set.singleton alphaEntry) : map CallSite siteList'
      (_,cyc):tl -> -- In this case, we found a cycle
        (CallSite cyc):(map (CallSite . fst) tl)
    where unCallSite (CallSite c) = c
          siteList' = map unCallSite $ unCallSites css
          alphaEntry = makeNewAlpha aid
          -- A list of pairs, the snd of which is the union of all the fsts so
          -- far.
          totals = zip siteList' $ tail $ scanl Set.union Set.empty siteList'
          rest = dropWhile (not . Set.member alphaEntry . snd) totals
          aid = alphaId a
          css = alphaCallSites a

type AlphaSubstitutionEnv = (CellAlpha, Set AnyAlpha)

substituteAlphaHelper :: (Alpha a) => a -> Reader AlphaSubstitutionEnv a
substituteAlphaHelper a = do
  (newAlpha, forallVars) <- ask
  let newCallSites = makeCallSites newAlpha
  if not $ Set.member (alphaWeaken a) forallVars
    then return a
    -- The variable we are substituting should never have marked
    -- call sites.  The only places where polymorphic function
    -- constraints (forall constraints) are built are by the
    -- inference rules themselves (which have no notion of call
    -- sites) and the type replacement function (which does not
    -- replace forall-ed elements within a forall constraint).
    else assert ((length . unCallSites) css == 0) $
       return $ setAlphaCallSites a newCallSites
  where css = alphaCallSites a


instance Display (SomeAlpha a) where
  makeDoc (SomeAlpha i cSites) =
      char '\'' <> makeDoc i <> (
          let doc = makeDoc cSites in
          if not $ isEmpty doc
              then char '^' <> doc
              else empty)

instance Display AnyAlpha where
  makeDoc (AnyAlpha a) = makeDoc a

instance Display CallSites where
  makeDoc sites =
      let siteList = reverse $ unCallSites sites in
      if length siteList == 0
          then empty
          else brackets $ hcat $ punctuate (text ", ") $ map sDoc siteList
    where sDoc (CallSite set) =
              if Set.size set == 1
                  then makeDoc $ Set.findMin set
                  else makeDoc set

instance Display CallSite where
  makeDoc (CallSite set) = makeDoc set
