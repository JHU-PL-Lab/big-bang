module Language.BigBang.Types.Closure
(
)
where

import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Types.Types ((<:))
import Language.BigBang.Types.Types (Constraints)
import Language.BigBang.Types.UtilTypes (LabelName)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (mappend)

topLevelAssignable :: T.TauDownClosed -> T.TauChi -> Bool
topLevelAssignable tau chi = True

findTauDownOpen :: Constraints -> Constraints
findTauDownOpen = Set.fromAscList . catMaybes . map fn . Set.toAscList
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

findAlphaOnLeft :: Constraints -> Map T.Alpha (Set T.TauUpClosed)
findAlphaOnLeft = Map.unionsWith mappend . map fn . Set.toAscList
  where fn c = 
          case c of
            T.Subtype (T.TdcAlpha a) b -> Map.singleton a $ Set.singleton b
            _                          -> Map.empty

findLblAlphaOnLeft :: Constraints -> Map T.Alpha (Set (LabelName, T.TauUpClosed))
findLblAlphaOnLeft = Map.unionsWith mappend . map fn . Set.toAscList
  where fn c = 
          case c of
            T.Subtype (T.TdcLabel lbl (T.TdcAlpha a)) b ->
              Map.singleton a $ Set.singleton (lbl, b)
            _ -> Map.empty

findAlphaAmpPairs :: Constraints -> Map (T.Alpha, T.Alpha) (Set T.TauUpClosed)
findAlphaAmpPairs = Map.unionsWith mappend . map fn . Set.toAscList
  where fn c =
          case c of
            T.Subtype (T.TdcOnion (T.TdcAlpha a) (T.TdcAlpha b)) c ->
              Map.singleton (a,b) $ Set.singleton c
            _ -> Map.empty

transitivity :: Constraints -> Constraints
transitivity cs = Set.fromList $
                  concat $
                  Map.elems $
                  Map.intersectionWith subtypeCrossProduct lefts rights
  where tdoCs  = findTauDownOpen cs
        lefts  = findAlphaOnRight tdoCs
        rights = findAlphaOnLeft cs
        subtypeCrossProduct xs ys =
          [ x <: y | x <- Set.toList xs, y <- Set.toList ys ]

labels :: Constraints -> Constraints
labels cs = Set.fromList $
            concat $
            Map.elems $
            Map.intersectionWith fn lefts rights
  where tdoCs    = findTauDownOpen cs
        lefts    = findAlphaOnRight tdoCs
        rights   = findLblAlphaOnLeft cs
        fn xs ys =
          [ T.TdcLabel lbl x <: y | x <- Set.toList xs, (lbl, y) <- Set.toList ys ]

amps :: Constraints -> Constraints
amps cs = Set.fromList $ allTrans lefts $ Map.toList rights
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
