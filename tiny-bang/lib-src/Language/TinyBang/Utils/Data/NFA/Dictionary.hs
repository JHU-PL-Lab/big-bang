{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, TupleSections #-}

{-|
  Defines operations for the dictionary implementation of the NFA.
-}
module Language.TinyBang.Utils.Data.NFA.Dictionary
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
) where

import Control.Arrow
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Language.TinyBang.Utils.Data.NFA.Data
import qualified Language.TinyBang.Utils.Data.NFA.Utils as Utils

empty :: (Ord a) => DictionaryNfa a
empty =
  DictionaryNfa
    { dictInitialStates = IntSet.singleton 0
    , dictTransitions = IntMap.fromList
        [ (0, Map.empty)
        ]
    , dictAcceptingStates = IntSet.empty
    , dictNextState = 1
    }

emptyString :: (Ord a) => DictionaryNfa a
emptyString =
  DictionaryNfa
    { dictInitialStates = IntSet.singleton 0
    , dictTransitions = IntMap.fromList
        [ (0, Map.empty)
        ]
    , dictAcceptingStates = IntSet.singleton 0
    , dictNextState = 1
    }

singleton :: (Ord a) => a -> DictionaryNfa a
singleton sym =
  DictionaryNfa
    { dictInitialStates = IntSet.singleton 0
    , dictTransitions = IntMap.fromList
        [ (0, Map.singleton (Just sym) $ IntSet.singleton 1)
        , (1, Map.empty)
        ]
    , dictAcceptingStates = IntSet.singleton 1
    , dictNextState = 2
    }

oneOf :: (Ord a) => [a] -> DictionaryNfa a
oneOf syms =
  DictionaryNfa
    { dictInitialStates = IntSet.singleton 0
    , dictTransitions = IntMap.fromList
        [ (0, Map.fromList $ map ((,IntSet.singleton 1) . Just) syms)
        , (1, Map.empty)
        ]
    , dictAcceptingStates = IntSet.singleton 1
    , dictNextState = 2
    }

addSuffix :: (Ord a) => a -> DictionaryNfa a -> DictionaryNfa a
addSuffix sym nfa =
  let newTransitions = IntMap.fromList $ map mkTransition $ IntSet.toList $
                          dictAcceptingStates nfa in
  DictionaryNfa
    { dictInitialStates = dictInitialStates nfa
    , dictTransitions = 
        IntMap.unionWith (Map.unionWith IntSet.union)
          (dictTransitions nfa)
          newTransitions
    , dictAcceptingStates = newFinalStates
    , dictNextState = dictNextState nfa + 1
    }
  where
    newFinalStates = IntSet.singleton $ dictNextState nfa
    mkTransition n =
      (n, Map.singleton (Just sym) newFinalStates)

kleeneStar :: (Ord a) => DictionaryNfa a -> DictionaryNfa a
kleeneStar = optional . oneOrMore

oneOrMore :: (Ord a) => DictionaryNfa a -> DictionaryNfa a
oneOrMore nfa =
  -- Just add an epsilon edge from every accept state to every initial state
  let newTransitions =
        IntMap.fromList $ map (,Map.singleton Nothing $ dictInitialStates nfa) $
          IntSet.toList $ dictAcceptingStates nfa
  in
  DictionaryNfa
    { dictInitialStates = dictInitialStates nfa
    , dictTransitions =
        IntMap.unionWith (Map.unionWith IntSet.union)
          (dictTransitions nfa) newTransitions
          
    , dictAcceptingStates = dictAcceptingStates nfa
    , dictNextState = dictNextState nfa
    }

optional :: (Ord a) => DictionaryNfa a -> DictionaryNfa a
optional nfa =
  let newState = dictNextState nfa in
  DictionaryNfa
    { dictInitialStates = IntSet.singleton newState
    , dictTransitions =
        IntMap.unionWith (Map.unionWith IntSet.union)
          (dictTransitions nfa) $
            IntMap.singleton newState $ Map.singleton Nothing $
              dictInitialStates nfa
    , dictAcceptingStates = IntSet.insert newState $ dictAcceptingStates nfa
    , dictNextState = dictNextState nfa + 1
    }

concatenate :: (Ord a) => DictionaryNfa a -> DictionaryNfa a -> DictionaryNfa a
concatenate nfa1 nfa2pre =
  let nfa2 = adjustIndices (dictNextState nfa1) nfa2pre in
  let initStates =
        if IntSet.null $ dictInitialStates nfa1 `IntSet.intersection`
                         dictAcceptingStates nfa1
          then dictInitialStates nfa1
          else dictInitialStates nfa1 `IntSet.union` dictInitialStates nfa2
  in
  let acceptStates =
        if IntSet.null $ dictInitialStates nfa2 `IntSet.intersection`
                         dictAcceptingStates nfa2
          then dictAcceptingStates nfa2
          else dictAcceptingStates nfa1 `IntSet.union` dictAcceptingStates nfa2
  in
  let trans =
        IntMap.unionsWith (Map.unionWith IntSet.union)
          [ dictTransitions nfa1 -- The old left transitions
          , dictTransitions nfa2 -- The old right transitions
            -- An epsilon edge from each accepting state of nfa1 to each
            -- initial state of nfa2.
          , IntMap.fromList $
              map (,Map.singleton Nothing $ dictInitialStates nfa2) $
              IntSet.toList $ dictAcceptingStates nfa1
          ]
  in
  DictionaryNfa
    { dictInitialStates = initStates
    , dictTransitions = trans
    , dictAcceptingStates = acceptStates
    , dictNextState = dictNextState nfa2
    }

adjustIndices :: (Ord a) => Int -> DictionaryNfa a -> DictionaryNfa a
adjustIndices n nfa =
  DictionaryNfa
    { dictInitialStates = IntSet.map (+n) $ dictInitialStates nfa
    , dictTransitions =
        IntMap.fromList $ map ((+ n) *** Map.map (IntSet.map (+ n))) $
          IntMap.toList $ dictTransitions nfa
    , dictAcceptingStates = IntSet.map (+n) $ dictAcceptingStates nfa
    , dictNextState = dictNextState nfa + n
    }

union :: forall a. (Ord a)
      => DictionaryNfa a -> DictionaryNfa a -> DictionaryNfa a
union nfa1 nfa2 =
  let (nfaL, nfaR) = if dictNextState nfa1 < dictNextState nfa2
                        then (nfa1, nfa2) else (nfa2, nfa1) in
  let nfaR' = adjustIndices (dictNextState nfaL) nfaR in
  DictionaryNfa
    { dictInitialStates =
        dictInitialStates nfaL `IntSet.union` dictInitialStates nfaR'
    , dictTransitions =
        dictTransitions nfaL `IntMap.union` dictTransitions nfaR'
    , dictAcceptingStates =
        dictAcceptingStates nfaL `IntSet.union` dictAcceptingStates nfaR'
    , dictNextState = dictNextState nfaR'
    }

acceptingStatesFn :: (Ord a) => DictionaryNfa a -> Int -> Bool
acceptingStatesFn nfa st = st `IntSet.member` dictAcceptingStates nfa

transitionsFn :: (Show a, Ord a)
              => DictionaryNfa a -> Int -> Map (Maybe a) IntSet
transitionsFn nfa st =
  fromMaybe
    (error $ "Could not find state " ++ show st ++ " in NFA:" ++ show nfa) $
    IntMap.lookup st $ dictTransitions nfa

accept :: forall a. (Show a, Ord a) => DictionaryNfa a -> [a] -> Bool
accept =
  Utils.accept
    IntSet.empty
    IntSet.union
    IntSet.toList
    dictInitialStates
    acceptingStatesFn
    transitionsFn

isEmpty :: forall a. (Show a, Ord a) => DictionaryNfa a -> Bool
isEmpty =
  Utils.isEmpty
    IntSet.empty
    IntSet.null
    IntSet.union
    IntSet.difference
    IntSet.toList
    dictInitialStates
    acceptingStatesFn
    transitionsFn
