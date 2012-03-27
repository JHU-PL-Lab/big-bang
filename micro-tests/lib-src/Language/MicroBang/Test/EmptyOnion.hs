module Language.MicroBang.Test.EmptyOnion
( tests
)
where

import Language.MicroBang.Test.UtilFunctions

import qualified Language.MicroBang.Ast as A

-- TODO: Write quickcheck properties that the empty onion is the left and right identity.

tests :: (?debug :: Bool) => Test
tests = TestLabel "Tests about the empty onion" $ TestList
  [ lexParseEval "(&)"
                 [TokOpenParen, TokOnionCons, TokCloseParen]
                 (A.EmptyOnion)
                 (A.VEmptyOnion)
  ]