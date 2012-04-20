module Language.LittleBang.Test.Primitive.Int
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg

-- TODO: write prop_testInteger using quickcheck

testInteger :: (?conf :: Cfg.Config) => Integer -> Test
testInteger i = lexParseEval (show i)
                             [TokIntegerLiteral i]
                             (LA.PrimInt i)
                             (TA.VPrimInt i)

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Integer tests" $ TestList $
  [ testInteger 1234567890
  , testInteger (-1234567890)
  ] ++ map testInteger [-50, 50] ++
  [ fPars "1_1"
  , xPars "_1" (LA.Var $ lIdent "_1")
  , fPars "1_"
  , xLexs "0x1"
      [TokIntegerLiteral 0, TokIdentifier "x1"]
  , xLexs "-0"
      [TokIntegerLiteral 0]
  ]
