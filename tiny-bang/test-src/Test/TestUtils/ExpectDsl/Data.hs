module Test.TestUtils.ExpectDsl.Data
( Expectation(..)
, DeepOnionPredicate
) where

import Language.TinyBang.Interpreter.DeepValues

data Expectation
  = Pass
      DeepOnionPredicate -- ^ The predicate that the result must match
      String -- ^ The original source of the predicate (for display purposes)
  | TypeFailure

-- |The type of predicate function produced by this module.
type DeepOnionPredicate = DeepOnion -> Bool

