{-# LANGUAGE ScopedTypeVariables #-}

{-|
  Contains utility functions for the definitions of NFA data type operations.
  These functions are defined without any particular data type in mind.
-}
module Language.TinyBang.Utils.Data.NFA.Utils
( accept
, isEmpty
) where

import Data.Map (Map)
import qualified Data.Map as Map

accept :: forall nfa stS st sy. (Ord sy)
       => stS
       -> (stS -> stS -> stS)
       -> (stS -> [st])
       -> (nfa -> stS)
       -> (nfa -> st -> Bool)
       -> (nfa -> st -> Map (Maybe sy) stS)
       -> nfa
       -> [sy]
       -> Bool
accept
    emptyStateSet
    stateSetUnion
    stateSetToList
    initStatesFn
    acceptingStateFn
    transitionsFn
    nfa
    str =
  any (testAccept emptyStateSet str) $ stateSetToList $ initStatesFn nfa
  where
    testAccept :: stS -> [sy] -> st -> Bool
    testAccept avoid input state =
      let epsilonStates = follow state Nothing in
      let epsilonSatisfy =
            any (testAccept (avoid `stateSetUnion` epsilonStates) input) $
              stateSetToList epsilonStates in
      let consumeSatisfy = case input of
            [] -> acceptingStateFn nfa state
            sy:input' ->
              any (testAccept emptyStateSet input') $
                stateSetToList $ follow state $ Just sy in
      consumeSatisfy || epsilonSatisfy
      where
        follow :: st -> Maybe sy -> stS
        follow st msy =
          Map.findWithDefault emptyStateSet msy $ transitionsFn nfa st

isEmpty :: forall nfa st stS sy.
           stS
        -> (stS -> Bool)
        -> (stS -> stS -> stS)
        -> (stS -> stS -> stS)
        -> (stS -> [st])
        -> (nfa -> stS)
        -> (nfa -> st -> Bool)
        -> (nfa -> st -> Map (Maybe sy) stS)
        -> nfa
        -> Bool
isEmpty
    emptyStateSet
    stateSetNull
    stateSetUnion
    stateSetDifference
    stateSetToList
    initialStatesFn
    acceptingStateFn
    transitionsFn
    nfa =
  not $ any (acceptingStateFn nfa) $ stateSetToList $
    explore (initialStatesFn nfa) emptyStateSet
  where
    explore :: stS -> stS -> stS
    explore toVisit visited =
      let reachable = foldr stateSetUnion emptyStateSet $ concatMap Map.elems $
            map (transitionsFn nfa) $ stateSetToList toVisit
      in
      let newStates = stateSetDifference reachable visited in
      let oldStates = stateSetUnion visited toVisit in
      if stateSetNull newStates
        then oldStates
        else explore newStates oldStates
