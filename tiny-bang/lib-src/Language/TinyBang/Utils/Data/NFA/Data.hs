{-# LANGUAGE GADTs, ScopedTypeVariables #-}

{-|
  Defines the core data types for the NFA implementation used in the TinyBang
  type system.
-}
module Language.TinyBang.Utils.Data.NFA.Data
( Nfa(..)
, DictionaryNfa(..)
, FunctionNfa(..)
) where

import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Set (Set)

-- |Defines the internal data type for NFAs.  This data type has two
--  constructors: one representing an NFA specified by a finite transition map
--  and one specified by functions.  The former is used whenever possible
--  because most operations are more efficient in that form; the NFA is
--  converted to the latter when an operation is extremely inefficient in that
--  form (e.g. subtraction).  Due to the use cases of the TinyBang type system,
--  the function form is never converted back.
data Nfa symbol where
  DictNfa :: (Ord symbol) => DictionaryNfa symbol -> Nfa symbol
  FuncNfa :: (Ord state, Ord symbol) => FunctionNfa state symbol -> Nfa symbol

-- |Defines the dictionary form of an NFA.  The initial and accepting states
--  are multiple (rather than using epsilons from a single start or end state)
--  for performance reasons.
data DictionaryNfa symbol where
  DictionaryNfa :: (Ord symbol) =>
      { dictInitialStates :: IntSet
      , dictTransitions :: IntMap (Map (Maybe symbol) IntSet)
      , dictAcceptingStates :: IntSet
      , dictNextState :: Int
      }
    -> DictionaryNfa symbol

-- |Defines the dictionary form of an NFA.  The initial and accepting states
--  are multiple (rather than using epsilons from a single start or end state)
--  for performance reasons.
data FunctionNfa state symbol where
  FunctionNfa :: (Ord state, Ord symbol) =>
      { funInitialStates :: Set state
      , funIsAcceptingState :: state -> Bool
      , funTransitions :: state -> Map (Maybe symbol) (Set state)
      }
    -> FunctionNfa state symbol
