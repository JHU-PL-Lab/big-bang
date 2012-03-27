module Language.MicroBang.Test.Primitive.Unit
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Ast as A

tests :: (?debug :: Bool) => Test
tests = TestLabel "Tests for unit" $ TestList
  [ lexParseEval "()"
                 [TokOpenParen, TokCloseParen]
                 A.PrimUnit
                 A.VPrimUnit
  , lexParseEval "( )"
                 [TokOpenParen, TokCloseParen]
                 A.PrimUnit
                 A.VPrimUnit
  , xEval "case () of { unit -> () }"
          A.VPrimUnit
  ]