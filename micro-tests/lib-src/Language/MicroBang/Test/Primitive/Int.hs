module Language.MicroBang.Test.Primitive.Int
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Ast as A

-- TODO: write prop_testInteger using quickcheck

testInteger :: (?debug :: Bool) => Integer -> Test
testInteger i = lexParseEval (show i)
                             (A.PrimInt i)
                             (A.VPrimInt i)

tests :: (?debug :: Bool) => Test
tests = TestLabel "Integer tests" $ TestList $
  [ testInteger 1234567890
  , testInteger (-1234567890)
  ] ++ map testInteger [-50, 50] ++
  [ fPars "1_1"
  --, xPars "_1" (A.Var $ ident "_1")
  , fPars "1_"
  , xLexs "0x1"
      [TokIntegerLiteral 0, TokIdentifier "x1"]
  , xLexs "-0"
      [TokIntegerLiteral 0]
  ]