module Language.TinyBang.Test.Primitive.Unit
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A

tests = TestLabel "Integer tests" $ TestList
  [ lexParseEval "()"
                 [TokUnit]
                 A.PrimUnit
                 A.VPrimUnit
  , lexParseEval "( )"
                 [TokUnit]
                 A.PrimUnit
                 A.VPrimUnit
  , xEval "case () of { unit -> () }"
          A.VPrimUnit
  ]
