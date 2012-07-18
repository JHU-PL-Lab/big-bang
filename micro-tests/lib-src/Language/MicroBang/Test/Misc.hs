module Language.MicroBang.Test.Misc
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import Language.MicroBang.Test.NameUtils
  ( lblTrue
  , lblFalse
  )
import Language.MicroBang.Test.ExpressionUtils
  ( multiAppl
  )
import qualified Language.MicroBang.Ast as A

tests :: (?conf :: Bool) => Test
tests = TestLabel "Miscellaneous tests" $ TestList
  [ lexParseEval "`True ()"
                 (A.Label lblTrue A.PrimUnit)
                 (A.VLabel lblTrue A.VPrimUnit)
  , lexParseEval "`False ()"
                 (A.Label lblFalse A.PrimUnit)
                 ( A.VLabel lblFalse A.VPrimUnit)
  ]