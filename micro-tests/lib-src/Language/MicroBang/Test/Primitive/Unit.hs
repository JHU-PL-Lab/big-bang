module Language.MicroBang.Test.Primitive.Unit
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Ast as A

tests :: (?debug :: Bool) => Test
tests = TestLabel "Tests for unit" $ TestList
  [ lexParseEval "()"
                 A.PrimUnit
                 A.VPrimUnit
  , lexParseEval "( )"
                 A.PrimUnit
                 A.VPrimUnit
  , xEval "case () of { unit -> () }"
          A.VPrimUnit
  ]