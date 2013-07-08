{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-|
  This module provides functionality for representing and using non-finite
  automata.
  
  Some of this content was derived from or inspired by
  https://github.com/leonidas/codeblog/blob/master/2011/2011-12-18-haskell-nfa.md
-}

module Data.NFA
( Nfa(..)
, empty
, accept
, acceptAt
, createFromData
, createFromDataWithEpsilon
, union
, intersect
, Data.NFA.subtract
, isEmpty
) where

import Control.Monad (foldM,guard)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map) 
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Set (Set)

-- |Describes a nondeterministic finite automaton.
data Nfa state symbol =
  Nfa {
    -- |Specifies the states of this NFA which are considered initial.
    initialState :: state,
    -- |Specifies how symbols cause a state to transition to other states.  An
    --  empty list denotes a failure to transition.
    transition :: state -> symbol -> [state],
    -- |Determines which states in this NFA are accepting states.
    acceptingState :: state -> Bool,
    -- |Specifies a finite list of symbols which will transition to non-empty
    --  states from the specified state.  This function must obey the law that
    --  elem sy (validTransitions st)  iff  transition st sy /= [] 
    validTransitions :: state -> [symbol]
  }
  
-- | Defines an empty NFA.
empty :: (Ord symbol) => Nfa () symbol
empty = createFromData () [] []

-- | Determines whether or not a sequence of symbols is accepted by the
--   specified NFA.
accept :: Nfa state symbol -> [symbol] -> Bool
accept nfa@(Nfa _ _ a _) = any a . acceptAt nfa

-- | Determines at which states a given sequence of symbols is accepted by the
--   specified NFA.
acceptAt :: Nfa state symbol -> [symbol] -> [state]
acceptAt (Nfa i t _ _) = foldM t i

-- | Creates an NFA from finite data.  The transitions are specified as triples
--   of states, symbols, and resulting states.
createFromData :: forall state symbol. (Eq state, Ord state, Ord symbol)
               => state -> [(state,symbol,state)] -> [state] -> Nfa state symbol
createFromData i edges accepts =
  Nfa i t a v
  where
    base :: forall k v. (Ord k)
         => ((state,symbol,state) -> k)
         -> ((state,symbol,state) -> v)
         -> k -> [v]
    base f g arg = Map.findWithDefault [] arg theMap
      where
        theMap = foldl addToMap Map.empty edges
        addToMap :: Map k [v] -> (state,symbol,state) -> Map k [v]
        addToMap m x =
          let k = f x in
          let old = Map.findWithDefault [] k m in
          Map.insert k (g x : old) m
    t :: state -> symbol -> [state]
    t = curry $ base (\(x,y,_) -> (x,y)) (\(_,_,z) -> z)
    a :: state -> Bool
    a = flip elem accepts
    v :: state -> [symbol]
    v = base (\(x,_,_) -> x) (\(_,y,_) -> y)

-- | Creates an NFA from finite data.  The transitions are specified as triples
--   of states, symbols, and resulting states.  When a symbol is Nothing, it
--   represents an epsilon.  This function simply performs epsilon elimination
--   on the data and calls createFromData accordingly.
createFromDataWithEpsilon
    :: forall state symbol. (Eq state, Ord state, Eq symbol, Ord symbol)
    => state -> [(state,Maybe symbol,state)] -> [state] -> Nfa state symbol
createFromDataWithEpsilon i edges' accepts' =
  let accepts'' = Set.toList $ closeAccepts $ Set.fromList accepts' in
  let edges'' = [ (sIn,sym,sOut)
                | (sIn,Just sym,sOut) <- Set.toList $ closeEdges $
                      Set.fromList edges' ]
  in createFromData i edges'' accepts''
  where
    -- Infers new accept states based on how they can be reached from epsilon
    -- edges.
    closeAccepts :: Set state -> Set state
    closeAccepts accepts =
      let acceptsNew = Set.union accepts $ Set.fromList
            [ state | (state,Nothing,state') <- edges'
                    , state' `Set.member` accepts ]
      in
      if acceptsNew == accepts then accepts else closeAccepts acceptsNew
    -- Performs transitive closure over the graph's epsilon edges. 
    closeEdges :: Set (state,Maybe symbol,state)
               -> Set (state,Maybe symbol,state)
    closeEdges edges = 
      let edgesNew = Set.union edges $ Set.fromList $ do
            (sIn,sym,sOut) <- Set.toList edges
            guard $ isNothing sym
            (sIn',sym',sOut') <- Set.toList edges
            guard $ sOut == sIn'
            return (sIn,sym',sOut')
      in
      if edgesNew == edges then edges else closeEdges edgesNew
    
-- | Generates the union between two NFAs.  The resulting NFA will accept a
--   string whenever either NFA accepts that string.
-- TODO: rewrite to be a touch more efficient (e.g. redefine NFA to have
--       multiple start states)
union :: (Eq state, Eq state', Eq symbol)
      => Nfa state symbol -> Nfa state' symbol
      -> Nfa (Maybe state,Maybe state') symbol
union (Nfa i1 t1 a1 v1) (Nfa i2 t2 a2 v2) =
  Nfa i t a v
  where
    nothingIfEmpty x = if null x then [Nothing] else x
    i = (Just i1,Just i2)
    t (s1,s2) sy =
      let s1's = nothingIfEmpty $ maybe [] (map Just . flip t1 sy) s1 in
      let s2's = nothingIfEmpty $ maybe [] (map Just . flip t2 sy) s2 in
      List.delete (Nothing,Nothing)
        [(s1',s2') | s1' <- s1's, s2' <- s2's]
    a (s1,s2) = maybe False a1 s1 || maybe False a2 s2
    v (s1,s2) = List.nub $ maybe [] v1 s1 ++ maybe [] v2 s2 

-- | Generates the intersection between two NFAs.  The resulting NFA will accept
--   a string only if both NFAs accept that string.
intersect :: (Eq symbol)
          => Nfa state symbol -> Nfa state' symbol -> Nfa (state,state') symbol
intersect (Nfa i1 t1 a1 v1) (Nfa i2 t2 a2 v2) =
  Nfa i t a v
  where
    i = (i1,i2)
    t (s1,s2) sy = [(s1',s2') | s1' <- t1 s1 sy, s2' <- t2 s2 sy]
    a (s1,s2) = a1 s1 && a2 s2
    v (s1,s2) = v1 s1 `List.intersect` v2 s2
    
-- | Subtracts one NFA from another.  The resulting NFA will accept only strings
--   which are accepted by the first NFA and not by the second.
subtract :: forall state state' symbol. (Eq state, Eq state', Eq symbol)
         => Nfa state symbol -> Nfa state' symbol
         -> Nfa (state,Maybe state') symbol
subtract (Nfa i1 t1 a1 v1) (Nfa i2 t2 a2 _) =
  Nfa i t a v
  where
    i = (i1,Just i2)
    t (s1,s2) sy =
      let s1's = t1 s1 sy in
      let s2's = maybe [] (`t2` sy) s2 in
      if null s2's
        then map (,Nothing) s1's
        else [(s1',Just s2') | s1' <- s1's, s2' <- s2's]
    a (s1,s2) = a1 s1 && not (maybe False a2 s2)
    v (s1,_) = v1 s1

-- | Determines whether or not the provided NFA is empty: that is, if it will
--   not accept any string.
isEmpty :: forall state symbol. (Ord state) => Nfa state symbol -> Bool
isEmpty (Nfa i t a v) = not $ explore Set.empty i
  where
    -- explore determines if an accept state is reachable
    explore :: Ord state => Set state -> state -> Bool
    explore visited current =
      let nextStates' = concatMap (t current) (v current)
          nextStates = Set.toList $ Set.difference (Set.fromList nextStates')
                          visited
          newVisited = Set.union visited $ Set.fromList nextStates
      in
      -- succeed if this is an accept state or anything we can reach from here
      -- is
      (a current || any (explore newVisited) nextStates)
