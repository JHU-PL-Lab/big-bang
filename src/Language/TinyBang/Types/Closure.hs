{-# LANGUAGE TupleSections #-}
module Language.TinyBang.Types.Closure
( calculateClosure
) where

import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.Types ( (<:)
                                     , (.:)
                                     , Constraints
                                     , Constraint
                                     )
import Language.TinyBang.Types.UtilTypes (LabelName)

import Data.Function.Utils (leastFixedPoint)
import Data.Maybe.Utils (justIf)
import Data.Set.Utils (singIf)

import Control.Exception (assert)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromJust, mapMaybe, listToMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (mappend)
import Control.Monad.Reader (runReader, ask, local, Reader)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)

-- |A function modeling immediate compatibility.  This function takes a type and
--  a guard in a match case.  If the input type is compatible with the guard,
--  this function returns the type as which the original type is compatible;
--  otherwise, Nothing is returned.  This function is equivalent to the
--  _ <:: _ ~ _ relation in the documentation.
immediatelyCompatible :: Constraints
                      -> T.TauDown
                      -> T.TauChi
                      -> Set T.TauDown
immediatelyCompatible constraints tau chi =
-- TODO: make this return Maybe
    case (tau,chi) of
        (_,T.ChiAny) -> Set.singleton tau
        (T.TdPrim p, T.ChiPrim p') -> tau `singIf` (p == p')
        (T.TdLabel n t, T.ChiLabel n' a) -> tau `singIf` (n == n')
        (T.TdOnion t1 t2, _) ->
            let mc1 = immediatelyCompatible constraints t1 chi in
            let mc2 = immediatelyCompatible constraints t2 chi in
            if Set.null mc2 then mc1 else mc2
        (T.TdFunc _, T.ChiFun) -> Set.singleton tau
        (T.TdAlpha a, _) -> Set.empty
-- TODO: Deal with lops

-- |A function modeling TLabelBind.  This function creates an appropriate set
--  of constraints to add when a given case branch is taken.  Its primary
--  purpose is to bind a label variable (such as `A x) to the contents of the
--  input.
tCaseBind :: T.ConstraintHistory
          -> T.TauDown
          -> T.TauChi
          -> Constraints
tCaseBind history tau chi =
    case (tau,chi) of
        (T.TdLabel n tau', T.ChiLabel n' a) ->
            (tau' <: T.TuAlpha a .: history)
                `singIf` (n == n')
        _ -> Set.empty

findAlphaOnRight :: Constraints
                 -> Map T.Alpha (Set (T.TauDown, Constraint))
findAlphaOnRight = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype a (T.TuAlpha b) _ ->
              Map.singleton b $ Set.singleton (a, c)
            _ -> Map.empty

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

findPolyFuncs :: Constraints
              -> Map T.Alpha (Set (T.Alpha, T.PolyFuncData, Constraint))
findPolyFuncs = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype (T.TdFunc pfd) (T.TuFunc ai ao) _ ->
                Map.singleton ai $ Set.singleton (ao, pfd, c)
            _ -> Map.empty

-- findAlphaAmpPairs :: Constraints
--                   -> Map (T.Alpha, T.Alpha) (Set ( T.TauUp
--                                                  , Constraint))
-- findAlphaAmpPairs = Map.unionsWith mappend . map fn . Set.toList
--   where fn c =
--           case c of
--             T.Subtype (T.TdOnion (T.TdAlpha a) (T.TdAlpha b)) d _ ->
--               Map.singleton (a,b) $ Set.singleton (d, c)
--             _ -> Map.empty

findCases :: Constraints -> Map T.Alpha (Set ([T.Guard], Constraint))
findCases = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Case a gs _ -> Map.singleton a $ Set.singleton (gs, c)
            _ -> Map.empty

-- |This function transforms a specified alpha into a call site list.  The
--  resulting call site list is in the reverse order form dictated by the
--  CallSites structure; that is, the list [{'3},{'2},{'1}] represents the type
--  variable with the exponent expression '1^('2^'3).  The resulting call site
--  list is suitable for use in type variable substitution for polymorphic
--  functions.
makeCallSites :: T.Alpha -> T.CallSites
makeCallSites alpha@(T.Alpha alphaId siteList) =
    T.callSites $
    case rest of
      [] -> -- In this case, this call site is new to the list
        (T.CallSite $ Set.singleton alphaEntry) : map T.CallSite siteList'
      (_,cyc):tl -> -- In this case, we found a cycle
        (T.CallSite cyc):(map (T.CallSite . fst) tl)
    where unCallSite (T.CallSite a) = a
          siteList' = map unCallSite $ T.unCallSites siteList
          alphaEntry = T.Alpha alphaId $ T.callSites []
          -- A list of pairs, the snd of which is the union of all the fsts so
          -- far.
          totals :: [(Set T.Alpha, Set T.Alpha)]
          totals = zip siteList' $ tail $ scanl Set.union Set.empty siteList'
          rest = dropWhile (not . Set.member alphaEntry . snd) totals

-- |A function which performs substitution on a set of constraints.  All
--  variables in the alpha set are replaced with corresponding versions that
--  have the specified alpha in their call sites list.
substituteVars :: Constraints -> Set T.Alpha -> T.Alpha -> Constraints
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

type AlphaSubstitutionEnv = (T.Alpha, Set T.Alpha)

-- |A typeclass for entities which can substitute their type variables.
class AlphaSubstitutable a where
  -- |The alpha in the reader environment is added to superscripts.
  --  The set in the reader environment contains alphas to ignore.
  substituteAlpha :: a -> Reader AlphaSubstitutionEnv a

instance AlphaSubstitutable T.Alpha where
  substituteAlpha alpha@(T.Alpha alphaId callSites) = do
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
      else assert ((length . T.unCallSites) callSites == 0) $
         return $ T.Alpha alphaId newCallSites

instance AlphaSubstitutable T.TauUp where
  substituteAlpha tau =
    case tau of
      T.TuFunc ai ao ->
        T.TuFunc
          <$> substituteAlpha ai
          <*> substituteAlpha ao
      T.TuAlpha a -> T.TuAlpha <$> substituteAlpha a

instance AlphaSubstitutable T.TauDown where
  substituteAlpha tau =
    case tau of
      T.TdPrim p -> return $ T.TdPrim p
      T.TdLabel n t -> T.TdLabel n <$> substituteAlpha t
      T.TdOnion t1 t2 ->
        T.TdOnion
          <$> substituteAlpha t1
          <*> substituteAlpha t2
      T.TdFunc pfd -> T.TdFunc <$> substituteAlpha pfd
      T.TdAlpha a -> T.TdAlpha <$> substituteAlpha a

instance AlphaSubstitutable T.PolyFuncData where
  substituteAlpha (T.PolyFuncData alphas alphaIn alphaOut constraints) =
      T.PolyFuncData alphas
        <$> substituteAlpha' alphaIn
        <*> substituteAlpha' alphaOut
        <*> substituteAlpha' constraints
      -- The variables described by the forall list should never be replaced
      where substituteAlpha' :: (AlphaSubstitutable a)
                             => a -> Reader AlphaSubstitutionEnv a
            substituteAlpha' =
              local (second $ flip Set.difference alphas) . substituteAlpha

instance AlphaSubstitutable T.Constraint where
  substituteAlpha c = case c of
      T.Subtype td tu hist ->
        T.Subtype
          <$> substituteAlpha td
          <*> substituteAlpha tu
          <*> return hist
      T.Case a guards hist ->
        T.Case
          <$> substituteAlpha a
          <*> mapM substituteAlpha guards
          <*> return hist
      T.Bottom hist -> return $ T.Bottom hist

instance AlphaSubstitutable T.Guard where
  substituteAlpha (T.Guard tauChi constraints) =
      T.Guard
        <$> substituteAlpha tauChi
        <*> substituteAlpha constraints

instance AlphaSubstitutable T.TauChi where
  substituteAlpha c = case c of
      T.ChiPrim p -> return $ T.ChiPrim p
      T.ChiLabel n a -> T.ChiLabel n <$> substituteAlpha a
      T.ChiFun -> return T.ChiFun
      T.ChiAny -> return T.ChiAny

instance (Ord a, AlphaSubstitutable a) => AlphaSubstitutable (Set a) where
  substituteAlpha = fmap Set.fromList . mapM substituteAlpha . Set.toList