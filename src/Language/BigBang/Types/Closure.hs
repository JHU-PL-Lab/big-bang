module Language.BigBang.Types.Closure
(
) where

import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Types.Types ((<:))
import Language.BigBang.Types.Types (Constraints)
import Language.BigBang.Types.UtilTypes (LabelName)
import Data.Maybe.Utils (justIf, safeHead)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, maybe, mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (mappend)

-- |A function which checks immediate compatability and produces an appropriate
--  constraint set for matches.  This function takes the type of a value, the
--  guard in a match case, and produces a constraint result.  If the result is
--  Nothing, the type is not compatible with the guard; otherwise, the set
--  provided should be added to the constraint set if this branch is chosen.
--  This function corresponds both to the relation <:: and to the mu function
--  in the notation.
createMatchConstraints :: T.TauDownOpen -> T.TauChi -> Maybe Constraints
createMatchConstraints tau chi =
    case (tau,chi) of
        (_,T.ChiTop) -> Just Set.empty
        (T.TdoPrim p, T.ChiPrim p') -> Set.empty `justIf` (p == p')
        (T.TdoLabel n t, T.ChiLabel n' a) ->
            let constraint = (T.toTauDownClosed t <: T.TucAlphaUp a) in
            (Set.singleton constraint) `justIf` (n == n')
        (T.TdoFunc _ _ _ _, T.ChiFun) -> Just Set.empty
        (T.TdoOnion t t', _) ->
            let mc1 = createMatchConstraints t chi in
            let mc2 = createMatchConstraints t' chi in
            maybe mc2 Just mc1

findTauDownOpen :: Constraints -> Constraints
findTauDownOpen = Set.fromList . catMaybes . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype a b -> fmap (const c) $ T.toTauDownOpen a
            _             -> Just c

findAlphaOnRight :: Constraints -> Map T.Alpha (Set T.TauDownClosed)
findAlphaOnRight = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype a (T.TucAlpha b) -> Map.singleton b $ Set.singleton a
            _                          -> Map.empty

findAlphaUpOnRight :: Constraints -> Map T.AlphaUp (Set T.TauDownClosed)
findAlphaUpOnRight = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype a (T.TucAlphaUp b) -> Map.singleton b $ Set.singleton a
            _                            -> Map.empty

findAlphaOnLeft :: Constraints -> Map T.Alpha (Set T.TauUpClosed)
findAlphaOnLeft = Map.unionsWith mappend . map fn . Set.toList
  where fn c = 
          case c of
            T.Subtype (T.TdcAlpha a) b -> Map.singleton a $ Set.singleton b
            _                          -> Map.empty

findLblAlphaOnLeft :: Constraints -> Map T.Alpha (Set (LabelName, T.TauUpClosed))
findLblAlphaOnLeft = Map.unionsWith mappend . map fn . Set.toList
  where fn c = 
          case c of
            T.Subtype (T.TdcLabel lbl (T.TdcAlpha a)) b ->
              Map.singleton a $ Set.singleton (lbl, b)
            _ -> Map.empty

findAlphaAmpPairs :: Constraints -> Map (T.Alpha, T.Alpha) (Set T.TauUpClosed)
findAlphaAmpPairs = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype (T.TdcOnion (T.TdcAlpha a) (T.TdcAlpha b)) c ->
              Map.singleton (a,b) $ Set.singleton c
            _ -> Map.empty

findCases :: Constraints -> Map T.AlphaUp [T.Guard]
findCases = Map.unionsWith uError . map fn . Set.toList
  where fn c =
          case c of
            T.Case au gs -> Map.singleton au gs
            _ -> Map.empty
        uError = error
            "constraint set contains two case constraints with same alphaUp"

closeTransitivity :: Constraints -> Constraints
closeTransitivity cs = Set.fromList $
                  concat $
                  Map.elems $
                  Map.intersectionWith subtypeCrossProduct lefts rights
  where tdoCs  = findTauDownOpen cs
        lefts  = findAlphaOnRight tdoCs
        rights = findAlphaOnLeft cs
        subtypeCrossProduct xs ys =
          [ x <: y | x <- Set.toList xs, y <- Set.toList ys ]

closeLabels :: Constraints -> Constraints
closeLabels cs = Set.fromList $
            concat $
            Map.elems $
            Map.intersectionWith fn lefts rights
  where tdoCs    = findTauDownOpen cs
        lefts    = findAlphaOnRight tdoCs
        rights   = findLblAlphaOnLeft cs
        fn xs ys =
          [ T.TdcLabel lbl x <: y | x <- Set.toList xs, (lbl, y) <- Set.toList ys ]

closeOnions :: Constraints -> Constraints
closeOnions cs = Set.fromList $ allTrans lefts $ Map.toList rights
  where tdoCs  = findTauDownOpen cs
        lefts  = findAlphaOnRight tdoCs
        rights = findAlphaAmpPairs cs
        tryTrans alphas ((a1, a2), tucs) = do
          t1 <- Map.lookup a1 alphas
          t2 <- Map.lookup a2 alphas
          return
            [ T.TdcOnion t1' t2' <: tuc |
              t1' <- Set.toList t1,
              t2' <- Set.toList t2,
              tuc <- Set.toList tucs]
        allTrans alphas amps = concat $ catMaybes $ map (tryTrans alphas) amps

closeCases :: Constraints -> Constraints
closeCases cs = Set.unions $ map pickGuardConstraints tausToGuards
  where lefts = findAlphaUpOnRight $ findTauDownOpen cs
        cases = findCases cs
        tausToGuards = Map.elems $
                Map.intersectionWith (,) lefts cases
        pickGuardConstraints (tauDownOpens, guards) =
            let resultConstraints = catMaybes
                    [ fmap (Set.union constr) $
                          createMatchConstraints tauDownOpen pat
                    | T.Guard pat constr <- guards
                    , tauDownOpen <- mapMaybe T.toTauDownOpen $
                          Set.toList tauDownOpens ]
            in
            maybe (Set.singleton T.Bottom) id $ safeHead resultConstraints

