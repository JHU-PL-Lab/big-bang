{-|
  This module defines an NFA library for the TinyBang type system.  The internal
  structure of the NFA type is designed to operate efficiently in use cases of
  the TinyBang type checker.
-}
module Language.TinyBang.Utils.Data.NFA
( Nfa

, empty
, emptyString
, singleton
, kleeneSingleton
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
import Language.TinyBang.Utils.Data.NFA.Generalized
