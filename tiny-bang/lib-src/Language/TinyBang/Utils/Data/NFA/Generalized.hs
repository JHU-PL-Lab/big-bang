{-# LANGUAGE Rank2Types #-}

{-|
  This module defines a set of generalized operations over the @Nfa@ data type.
  It transitions from one implementation to another as necessary and attempts
  to preserve efficiency.
-}
module Language.TinyBang.Utils.Data.NFA.Generalized
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
, Language.TinyBang.Utils.Data.NFA.Generalized.subtract
, intersect
) where

import Language.TinyBang.Utils.Data.NFA.Data
import qualified Language.TinyBang.Utils.Data.NFA.Dictionary as Dict
import qualified Language.TinyBang.Utils.Data.NFA.Function as Func

-- |An NFA accepting nothing.
empty :: (Ord a) => Nfa a
empty = DictNfa Dict.empty

-- |An NFA accepting just the empty string.
emptyString :: (Ord a) => Nfa a
emptyString = DictNfa Dict.emptyString

-- |An NFA accepting just a string composed of the specified character.
singleton :: (Ord a) => a -> Nfa a
singleton = DictNfa . Dict.singleton

-- |An NFA accepting any single-character string such that the character is one
--  of those specified.
oneOf :: (Ord a) => [a] -> Nfa a
oneOf = DictNfa . Dict.oneOf

addSuffix :: (Ord a) => a -> Nfa a -> Nfa a
addSuffix sym = nfa1Fn (Dict.addSuffix sym) (FuncNfa . Func.addSuffix sym)

kleeneStar :: (Ord a) => Nfa a -> Nfa a
kleeneStar = optional . oneOrMore

oneOrMore :: (Ord a) => Nfa a -> Nfa a
oneOrMore = nfa1Fn Dict.oneOrMore $ FuncNfa . Func.oneOrMore

optional :: (Ord a) => Nfa a -> Nfa a
optional = nfa1Fn Dict.optional $ FuncNfa . Func.optional

concatenate :: (Ord a) => Nfa a -> Nfa a -> Nfa a
concatenate = nfa2Fn Dict.concatenate $ \x y -> FuncNfa $ Func.concatenate x y

union :: (Ord a) => Nfa a -> Nfa a -> Nfa a
union = nfa2Fn Dict.union $ \x y -> FuncNfa $ Func.union x y

accept :: (Show a, Ord a) => Nfa a -> [a] -> Bool
accept nfa =
  case nfa of
    DictNfa dNfa -> Dict.accept dNfa
    FuncNfa fNfa -> Func.accept fNfa

isEmpty :: (Show a, Ord a) => Nfa a -> Bool
isEmpty nfa =
  case nfa of
    DictNfa dNfa -> Dict.isEmpty dNfa
    FuncNfa fNfa -> Func.isEmpty fNfa

-- |Subtracts the second NFA from the first NFA.
subtract :: (Ord a) => Nfa a -> Nfa a -> Nfa a
subtract = nfa2FnPromote $ \x y -> FuncNfa $ Func.subtract x y

intersect :: (Ord a) => Nfa a -> Nfa a -> Nfa a
intersect = nfa2FnPromote $ \x y -> FuncNfa $ Func.intersect x y

nfa1Fn :: (Ord a)
       => (DictionaryNfa a -> DictionaryNfa a)
       -> (forall st. (Ord st) => FunctionNfa st a -> Nfa a)
       -> Nfa a
       -> Nfa a
nfa1Fn dFn fFn nfa =
  case nfa of
    DictNfa dNfa -> DictNfa $ dFn dNfa
    FuncNfa fNfa -> fFn fNfa

nfa2Fn :: (Ord a)
       => (DictionaryNfa a -> DictionaryNfa a -> DictionaryNfa a)
       -> (forall st1 st2. (Ord st1, Ord st2) =>
            FunctionNfa st1 a -> FunctionNfa st2 a -> Nfa a)
       -> Nfa a
       -> Nfa a
       -> Nfa a
nfa2Fn dFn fFn nfa1 nfa2 =
  case (nfa1, nfa2) of
    (DictNfa dNfa1, DictNfa dNfa2) ->
      DictNfa $ dFn dNfa1 dNfa2
    (DictNfa dNfa1, FuncNfa fNfa2) ->
      fFn (Func.fromDictionaryNfa dNfa1) fNfa2
    (FuncNfa fNfa1, DictNfa dNfa2) ->
      fFn fNfa1 (Func.fromDictionaryNfa dNfa2)
    (FuncNfa fNfa1, FuncNfa fNfa2) ->
      fFn fNfa1 fNfa2

nfa2FnPromote :: (Ord a)
              => (forall st1 st2. (Ord st1, Ord st2) =>
                    FunctionNfa st1 a -> FunctionNfa st2 a -> Nfa a)
              -> Nfa a
              -> Nfa a
              -> Nfa a
nfa2FnPromote fFn nfa1 nfa2 =
  case (nfa1, nfa2) of
    (DictNfa dNfa1, DictNfa dNfa2) ->
      fFn (Func.fromDictionaryNfa dNfa1) (Func.fromDictionaryNfa dNfa2)
    (DictNfa dNfa1, FuncNfa fNfa2) ->
      fFn (Func.fromDictionaryNfa dNfa1) fNfa2
    (FuncNfa fNfa1, DictNfa dNfa2) ->
      fFn fNfa1 (Func.fromDictionaryNfa dNfa2)
    (FuncNfa fNfa1, FuncNfa fNfa2) ->
      fFn fNfa1 fNfa2
