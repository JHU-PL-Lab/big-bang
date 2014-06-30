{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

{-|
  Contains an NFA implementation using functions.  This library is less
  efficient than @Language.TinyBang.Utils.Data.NFA.Dictionary@ because
  arbitrarily extends the size of the state and relies on extensive closures,
  but it provides functionality that the dictionary model does not.
-}
module Language.TinyBang.Utils.Data.NFA.Function
( empty
, emptyString
, singleton
, oneOf
, addSuffix
, kleeneStar
, oneOrMore
, optional
, concatenate
, union
, accept
, isEmpty
, Language.TinyBang.Utils.Data.NFA.Function.subtract
, intersect
, fromDictionaryNfa
) where

import Control.Arrow
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Utils.Data.NFA.Data
import qualified Language.TinyBang.Utils.Data.NFA.Dictionary as Dict
import qualified Language.TinyBang.Utils.Data.NFA.Utils as Utils

empty :: (Ord sy) => FunctionNfa Int sy
empty =
  FunctionNfa
    { funInitialStates = Set.singleton (0 :: Int)
    , funIsAcceptingState = const False
    , funTransitions = const Map.empty
    }

emptyString :: (Ord sy) => FunctionNfa Int sy
emptyString =
  FunctionNfa
    { funInitialStates = Set.singleton (0 :: Int)
    , funIsAcceptingState = (== (0 :: Int))
    , funTransitions = const Map.empty
    }
    
singleton :: (Ord sy) => sy -> FunctionNfa Int sy
singleton = fromDictionaryNfa . Dict.singleton

oneOf :: (Ord sy) => [sy] -> FunctionNfa Int sy
oneOf = fromDictionaryNfa . Dict.oneOf

addSuffix :: (Ord sy)
          => sy -> FunctionNfa st sy -> FunctionNfa (Either st ()) sy
addSuffix sym (FunctionNfa initialStates isAcceptingState transition) =
  let newFinalState = Right () in
  FunctionNfa
    { funInitialStates = Set.map Left initialStates
    , funIsAcceptingState = (== newFinalState)
    , funTransitions = \st ->
        case st of
          Left st' ->
            let olds = Map.map (Set.map Left) $ transition st' in
            if isAcceptingState st'
              then Map.unionWith Set.union olds $
                    Map.singleton (Just sym) $ Set.singleton newFinalState
              else olds
          Right () ->
            Map.empty
    }

kleeneStar :: (Ord st, Ord sy) => FunctionNfa st sy -> FunctionNfa (Maybe st) sy
kleeneStar = optional . oneOrMore

oneOrMore :: (Ord st, Ord sy) => FunctionNfa st sy -> FunctionNfa st sy
oneOrMore nfa =
  FunctionNfa
    { funInitialStates = funInitialStates nfa
    , funIsAcceptingState = funIsAcceptingState nfa
    , funTransitions = \st ->
        let oldTrans = funTransitions nfa st in
        let newTransFn =
              if funIsAcceptingState nfa st
                then Map.unionWith Set.union
                      (Map.singleton Nothing $ funInitialStates nfa)
                else id
        in
        newTransFn oldTrans
    }

optional :: (Ord st, Ord sy) => FunctionNfa st sy -> FunctionNfa (Maybe st) sy
optional nfa =
  FunctionNfa
    { funInitialStates = Set.singleton Nothing
    , funIsAcceptingState = maybe True (funIsAcceptingState nfa)
    , funTransitions = \st -> case st of
        Nothing -> Map.singleton Nothing $ Set.map Just $ funInitialStates nfa
        Just st' -> Map.map (Set.map Just) $ funTransitions nfa st'
    }

concatenate :: (Ord st1, Ord st2, Ord sy)
            => FunctionNfa st1 sy
            -> FunctionNfa st2 sy
            -> FunctionNfa (Either st1 st2) sy
concatenate nfa1 nfa2 =
  FunctionNfa
    { funInitialStates = Set.map Left $ funInitialStates nfa1
    , funIsAcceptingState = either (const False) $ funIsAcceptingState nfa2
    , funTransitions = \st -> case st of
        Left st1 ->
          let oldTrans = Map.map (Set.map Left) $ funTransitions nfa1 st1 in
          let newTransFn =
                if funIsAcceptingState nfa1 st1
                  then Map.unionWith Set.union $ Map.singleton Nothing $
                          Set.map Right $ funInitialStates nfa2
                  else id
          in
          newTransFn oldTrans
        Right st2 ->
          Map.map (Set.map Right) $ funTransitions nfa2 st2
    }

union :: forall st1 st2 sy. (Ord sy)
      => FunctionNfa st1 sy
      -> FunctionNfa st2 sy
      -> FunctionNfa (Either st1 st2) sy
union (FunctionNfa is1 ias1 tr1) (FunctionNfa is2 ias2 tr2) =
  FunctionNfa
    { funInitialStates = Set.map Left is1 `Set.union` Set.map Right is2
    , funIsAcceptingState = either ias1 ias2
    , funTransitions =
        either (Map.map (Set.map Left) . tr1) (Map.map (Set.map Right) . tr2)
    }

accept :: forall st sy. (Ord st, Ord sy) => FunctionNfa st sy -> [sy] -> Bool
accept =
  Utils.accept
    Set.empty
    Set.union
    Set.toList
    (\(FunctionNfa is _ _) -> is)
    (\(FunctionNfa _ ias _) -> ias)
    (\(FunctionNfa _ _ tr) -> tr)

isEmpty :: forall st sy. (Ord st, Ord sy) => FunctionNfa st sy -> Bool
isEmpty =
  Utils.isEmpty
    Set.empty
    Set.null
    Set.union
    Set.difference
    Set.toList
    (\(FunctionNfa is _ _) -> is)
    (\(FunctionNfa _ ias _) -> ias)
    (\(FunctionNfa _ _ tr) -> tr)

epsilonFollowingTransition :: forall st sy. (Ord st, Ord sy)
                             => FunctionNfa st sy
                             -> st
                             -> Map sy (Set st)
epsilonFollowingTransition nfa st =
  examine (Set.singleton st) st
  where
    examine :: Set st -> st -> Map sy (Set st)
    examine avoid state =
      Map.unionsWith Set.union $ map extract $
        Map.toList $ funTransitions nfa state
      where
        extract :: (Maybe sy, Set st) -> Map sy (Set st)
        extract (msy, stS) = case msy of
          Just sy -> Map.singleton sy stS
          Nothing ->
            Map.unionsWith Set.union $
              map (examine $ avoid `Set.union` stS) $
                Set.toList $ stS `Set.difference` avoid
                
subtract :: forall st1 st2 sy. (Ord sy)
         => FunctionNfa st1 sy
         -> FunctionNfa st2 sy
         -> FunctionNfa (st1, Set st2) sy
subtract nfa1@(FunctionNfa is1 ias1 _) nfa2@(FunctionNfa is2 ias2 _) =
  -- We model each state as the product of the first NFA state and Maybe the
  -- second NFA state.  If the second state is not accepting or the second
  -- state is Nothing (due to an invalid transition of the second NFA), then
  -- an accepting state from the first NFA is still accepting in the result.
  FunctionNfa
    { funInitialStates = Set.map (,is2) is1
    , funIsAcceptingState = \st ->
        ias1 (fst st) && not (any ias2 $ Set.toList $ snd st)
    , funTransitions = \(st1, st2s) ->
        let m1 = epsilonFollowingTransition nfa1 st1 in
        let m2 = Map.unionsWith Set.union $
                  map (epsilonFollowingTransition nfa2) $ Set.toList st2s in
        Map.mapKeys Just $
          Map.mergeWithKey
            (\_ sts1' sts2' -> Just $ Set.map (,sts2') sts1')
            (Map.map $ Set.map (,Set.empty)) (const Map.empty) m1 m2
    }

intersect :: forall st1 st2 sy. (Ord sy)
          => FunctionNfa st1 sy
          -> FunctionNfa st2 sy
          -> FunctionNfa (st1, st2) sy
intersect nfa1@(FunctionNfa is1 ias1 _) nfa2@(FunctionNfa is2 ias2 _) =
  FunctionNfa
    { funInitialStates =
        Set.fromList [ (st1, st2)
                     | st1 <- Set.toList is1
                     , st2 <- Set.toList is2 ]
    , funIsAcceptingState = \st -> ias1 (fst st) && ias2 (snd st)
    , funTransitions = \(st1, st2) ->
        let m1 = epsilonFollowingTransition nfa1 st1 in
        let m2 = epsilonFollowingTransition nfa2 st2 in
        let joinStateSets ss1 ss2 =
              Set.fromList
                [ (st1',st2')
                | st1' <- Set.toList ss1
                , st2' <- Set.toList ss2
                ]
        in
        Map.mapKeys Just $ Map.intersectionWith joinStateSets m1 m2
    }
    
fromDictionaryNfa :: (Ord sy) => DictionaryNfa sy -> FunctionNfa Int sy
fromDictionaryNfa nfa =
  FunctionNfa
    { funInitialStates = Set.fromList $ IntSet.toList $ dictInitialStates nfa
    , funIsAcceptingState = (`IntSet.member` dictAcceptingStates nfa)
    , funTransitions = \st ->
        Map.findWithDefault Map.empty st $ Map.fromList $
          map (second (Map.map (Set.fromList . IntSet.toList))) $
            IntMap.toList $ dictTransitions nfa
    }
