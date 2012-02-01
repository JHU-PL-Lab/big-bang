module Language.TinyBang.Test.EmptyOnion
( tests
)
where

import Language.TinyBang.Test.UtilFunctions

import qualified Language.TinyBang.Ast as A

-- TODO: Write quickcheck properties that the empty onion is the left and right identity.

tests = TestLabel "Tests about the empty onion" $ TestList
  [ lexParseEval "(&)"
                 [TokOpenParen, TokOnionCons, TokCloseParen]
                 (A.EmptyOnion)
                 (A.VEmptyOnion)
  ]
