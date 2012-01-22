{-# LANGUAGE TupleSections #-}
module Language.TinyBang.Types.Closure
( calculateClosure
) where

import Language.TinyBang.Types.Types ( (<:)
                                     , (.:)
                                     , Constraints
                                     , Constraint(..)
                                     , TauDown(..)
                                     , TauUp(..)
                                     , TauChi(..)
                                     , ConstraintHistory
                                     , Alpha(..)
                                     , CallSite(..)
                                     , CallSites(..)
                                     , callSites
                                     , PolyFuncData(..)
                                     , Guard(..)
                                     )
import Language.TinyBang.Types.UtilTypes (LabelName)

import Data.Function.Utils (leastFixedPoint)
import Data.Maybe.Utils (justIf)
import Data.Set.Utils (singIf)

import Control.Exception (assert)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromJust, mapMaybe, listToMaybe, isJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (mappend)
import Control.Monad.Reader (runReader, ask, local, Reader)
import Control.Monad.Writer (tell, Writer, execWriter)
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, second, (***), (&&&))
import Data.Either (partitionEithers)

data Compatibility = NotCompatible | MaybeCompatible | CompatibleAs TauDown

-- |A function modeling immediate compatibility.  This function takes a type and
--  a guard in a match case.  If the input type is compatible with the guard,
--  this function returns @CompatibleAs t@, where t is the type as which the
--  original type is compatible; otherwise, @NotCompatible@ is
--  returned. MaybeCompatible is returned if the result is not yet determinable,
--  as in the case of lazy operations not yet being closed over.  This function
--  is equivalent to the _ <:: _ ~ _ relation in the documentation.

-- Note that lazy ops match against ChiAny; TODO: is this desired behavior?
immediatelyCompatible :: TauDown
                      -> TauChi
                      -> Compatibility
immediatelyCompatible tau chi =
  case (tau,chi) of
    (_,ChiAny) -> CompatibleAs tau
    (TdPrim p, ChiPrim p') | p == p' -> CompatibleAs tau
    (TdLabel n t, ChiLabel n' a) | n == n' -> CompatibleAs tau
    (TdOnion t1 t2, _) ->
      case (immediatelyCompatible t1 chi, immediatelyCompatible t2 chi) of
        (_, MaybeCompatible) -> MaybeCompatible
        (c, NotCompatible) -> c
        -- If we reach this case, we must have found compatibility in the second
        -- onion.
        (_, c) -> c
    (TdFunc _, ChiFun) -> CompatibleAs tau
    (TdLazyOp _ _ _, _) -> MaybeCompatible
    -- The line below is not strictly necessary, but included for clarity.
    (TdAlpha a, _) -> NotCompatible
    _ -> NotCompatible

-- |A function modeling TLabelBind.  This function creates an appropriate set of
--  constraints to add when a given case branch is taken.  Its primary purpose
--  is to bind a label variable (such as `A x) to the contents of the input.
tCaseBind :: ConstraintHistory
          -> TauDown
          -> TauChi
          -> Constraints
tCaseBind history tau chi =
    case (tau,chi) of
        (TdLabel n tau', ChiLabel n' a) ->
            (tau' <: TuAlpha a .: history)
                `singIf` (n == n')
        _ -> Set.empty

getLowerBound :: Constraint -> Maybe TauDown
getLowerBound c =
  case c of
    Subtype td _ _ -> Just td
    _ -> Nothing

getUpperBound :: Constraint -> Maybe TauUp
getUpperBound c =
  case c of
    Subtype _ tu _ -> Just tu
    _ -> Nothing

getHistory :: Constraint -> ConstraintHistory
getHistory c =
  case c of
    Subtype _ _ h -> h
    Case _ _ h -> h
    Bottom h -> h

filterByUpperBound :: Constraints -> TauUp -> Constraints
filterByUpperBound cs t = Set.filter f cs
  where f c = Just t == getUpperBound c

filterByLowerBound :: Constraints -> TauDown -> Constraints
filterByLowerBound cs t = Set.filter f cs
  where f c = Just t == getLowerBound c

--getByUpperBound :: Constraints -> T.TauUp -> Set (T.TauDown, T.ConstraintHistory)
--getByUpperBound cs t = Set.fromAscList $ mapMaybe

findConcreteLowerBounds :: Constraints -> TauUp -> Set TauDown
findConcreteLowerBounds cs t = execWriter $ loop [t]
  where toEither c =
          case c of
            TdAlpha a -> Left a
            _ -> Right c
        -- This is elegant, but is it too clever by half?
        -- As it stands, it can be optimized by removing used constraints
        step :: TauUp -> Writer (Set TauDown) [TauUp]
        step
          = uncurry (flip (>>))
          . (return . map TuAlpha *** tell . Set.fromAscList)
          . partitionEithers
          . map toEither
          . mapMaybe getLowerBound
          . Set.toAscList
          . filterByUpperBound cs
        loop :: [TauUp] -> Writer (Set TauDown) ()
        loop xs = do
          when (not $ null xs) $
            concat <$> mapM step (nub' xs) >>= loop
        nub' = Set.toList . Set.fromList

concretizeType :: Constraints -> TauDown -> Set TauDown
concretizeType cs t =
  case t of
    TdOnion t1 t2 -> Set.fromList
      [TdOnion a b | a <- Set.toList $ f t1
                   , b <- Set.toList $ f t2]
    TdAlpha a -> Set.unions $ map f $ lowerBounds a
    _ -> Set.singleton t
  where f = concretizeType cs
        lowerBounds :: Alpha -> [TauDown]
        lowerBounds alpha
          = mapMaybe getLowerBound
          $ Set.toList
          $ filterByUpperBound cs
          $ TuAlpha alpha

-- findAlphaOnRight :: Constraints
--                  -> Map T.Alpha (Set (T.TauDown, Constraint))
-- findAlphaOnRight = Map.unionsWith mappend . map fn . Set.toList
--   where fn c =
--           case c of
--             T.Subtype a (T.TuAlpha b) _ ->
--               Map.singleton b $ Set.singleton (a, c)
--             _ -> Map.empty

-- findAlphaOnLeft :: Constraints
--                 -> Map T.Alpha (Set (T.TauUpClosed, Constraint))
-- findAlphaOnLeft = Map.unionsWith mappend . map fn . Set.toList
--   where fn c =
--           case c of
--             T.Subtype (T.TdAlpha a) b _ -> Map.singleton a $
--                                               Set.singleton (b, c)
--             _                            -> Map.empty

-- findLblAlphaOnLeft :: Constraints
--                    -> Map T.Alpha (Set ( LabelName
--                                        , T.TauUpClosed
--                                        , Constraint))
-- findLblAlphaOnLeft = Map.unionsWith mappend . map fn . Set.toList
--   where fn c =
--           case c of
--             T.Subtype (T.TdLabel lbl (T.TdAlpha a)) b _ ->
--               Map.singleton a $ Set.singleton (lbl, b, c)
--             _ -> Map.empty

-- findPolyFuncs :: Constraints
--               -> Map T.Alpha (Set (T.Alpha, T.PolyFuncData, Constraint))
-- findPolyFuncs = Map.unionsWith mappend . map fn . Set.toList
--   where fn c =
--           case c of
--             T.Subtype (T.TdFunc pfd) (T.TuFunc ai ao) _ ->
--                 Map.singleton ai $ Set.singleton (ao, pfd, c)
--             _ -> Map.empty

-- findAlphaAmpPairs :: Constraints
--                   -> Map (T.Alpha, T.Alpha) (Set ( T.TauUp
--                                                  , Constraint))
-- findAlphaAmpPairs = Map.unionsWith mappend . map fn . Set.toList
--   where fn c =
--           case c of
--             T.Subtype (T.TdOnion (T.TdAlpha a) (T.TdAlpha b)) d _ ->
--               Map.singleton (a,b) $ Set.singleton (d, c)
--             _ -> Map.empty

findCases :: Constraints -> Map Alpha (Set ([Guard], Constraint))
findCases = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            Case a gs _ -> Map.singleton a $ Set.singleton (gs, c)
            _ -> Map.empty

-- |This function transforms a specified alpha into a call site list.  The
--  resulting call site list is in the reverse order form dictated by the
--  CallSites structure; that is, the list [{'3},{'2},{'1}] represents the type
--  variable with the exponent expression '1^('2^'3).  The resulting call site
--  list is suitable for use in type variable substitution for polymorphic
--  functions.
makeCallSites :: Alpha -> CallSites
makeCallSites alpha@(Alpha alphaId siteList) =
    callSites $
    case rest of
      [] -> -- In this case, this call site is new to the list
        (CallSite $ Set.singleton alphaEntry) : map CallSite siteList'
      (_,cyc):tl -> -- In this case, we found a cycle
        (CallSite cyc):(map (CallSite . fst) tl)
    where unCallSite (CallSite a) = a
          siteList' = map unCallSite $ unCallSites siteList
          alphaEntry = Alpha alphaId $ callSites []
          -- A list of pairs, the snd of which is the union of all the fsts so
          -- far.
          totals :: [(Set Alpha, Set Alpha)]
          totals = zip siteList' $ tail $ scanl Set.union Set.empty siteList'
          rest = dropWhile (not . Set.member alphaEntry . snd) totals

-- |A function which performs substitution on a set of constraints.  All
--  variables in the alpha set are replaced with corresponding versions that
--  have the specified alpha in their call sites list.
substituteVars :: Constraints -> Set Alpha -> Alpha -> Constraints
substituteVars constraints forallVars replAlpha =
  runReader
    (substituteAlpha constraints)
    (replAlpha, forallVars)

closeCases :: Constraints -> Constraints
closeCases cs = error "Not implemented"

closeApplications :: Constraints -> Constraints
closeApplications cs = error "Not implemented"

closeLops :: Constraints -> Constraints
closeLops cs = error "Not yet implemented"

-- |This closure calculation function produces appropriate bottom values for
--  immediate contradictions (such as tprim <: tprim' where tprim != tprim').
closeSingleContradictions :: Constraints -> Constraints
closeSingleContradictions cs = error "Not yet implemented"

closeAll :: Constraints -> Constraints
closeAll c = Set.unions $ map ($ c)
        [ id
        , closeCases
        , closeApplications
        , closeLops
        ]

-- |Calculates the transitive closure of a set of type constraints.
calculateClosure :: Constraints -> Constraints
calculateClosure c = leastFixedPoint closeAll c

type AlphaSubstitutionEnv = (Alpha, Set Alpha)

-- |A typeclass for entities which can substitute their type variables.
class AlphaSubstitutable a where
  -- |The alpha in the reader environment is added to superscripts.
  --  The set in the reader environment contains alphas to ignore.
  substituteAlpha :: a -> Reader AlphaSubstitutionEnv a

instance AlphaSubstitutable Alpha where
  substituteAlpha alpha@(Alpha alphaId callSites) = do
    (newAlpha, forallVars) <- ask
    let newCallSites = makeCallSites newAlpha
    if not $ Set.member alpha forallVars
      then return alpha
      -- The variable we are substituting should never have marked
      -- call sites.  The only places where polymorphic function
      -- constraints (forall constraints) are built are by the
      -- inference rules themselves (which have no notion of call
      -- sites) and the type replacement function (which does not
      -- replace forall-ed elements within a forall constraint).
      else assert ((length . unCallSites) callSites == 0) $
         return $ Alpha alphaId newCallSites

instance AlphaSubstitutable TauUp where
  substituteAlpha tau =
    case tau of
      TuFunc ai ao ->
        TuFunc
          <$> substituteAlpha ai
          <*> substituteAlpha ao
      TuAlpha a -> TuAlpha <$> substituteAlpha a

instance AlphaSubstitutable TauDown where
  substituteAlpha tau =
    case tau of
      TdPrim p -> return $ TdPrim p
      TdLabel n t -> TdLabel n <$> substituteAlpha t
      TdOnion t1 t2 ->
        TdOnion
          <$> substituteAlpha t1
          <*> substituteAlpha t2
      TdFunc pfd -> TdFunc <$> substituteAlpha pfd
      TdAlpha a -> TdAlpha <$> substituteAlpha a

instance AlphaSubstitutable PolyFuncData where
  substituteAlpha (PolyFuncData alphas alphaIn alphaOut constraints) =
      PolyFuncData alphas
        <$> substituteAlpha' alphaIn
        <*> substituteAlpha' alphaOut
        <*> substituteAlpha' constraints
      -- The variables described by the forall list should never be replaced
      where substituteAlpha' :: (AlphaSubstitutable a)
                             => a -> Reader AlphaSubstitutionEnv a
            substituteAlpha' =
              local (second $ flip Set.difference alphas) . substituteAlpha

instance AlphaSubstitutable Constraint where
  substituteAlpha c = case c of
      Subtype td tu hist ->
        Subtype
          <$> substituteAlpha td
          <*> substituteAlpha tu
          <*> return hist
      Case a guards hist ->
        Case
          <$> substituteAlpha a
          <*> mapM substituteAlpha guards
          <*> return hist
      Bottom hist -> return $ Bottom hist

instance AlphaSubstitutable Guard where
  substituteAlpha (Guard tauChi constraints) =
      Guard
        <$> substituteAlpha tauChi
        <*> substituteAlpha constraints

instance AlphaSubstitutable TauChi where
  substituteAlpha c = case c of
      ChiPrim p -> return $ ChiPrim p
      ChiLabel n a -> ChiLabel n <$> substituteAlpha a
      ChiFun -> return ChiFun
      ChiAny -> return ChiAny

instance (Ord a, AlphaSubstitutable a) => AlphaSubstitutable (Set a) where
  substituteAlpha = fmap Set.fromList . mapM substituteAlpha . Set.toList
