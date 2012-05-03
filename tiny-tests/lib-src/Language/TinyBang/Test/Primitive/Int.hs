module Language.TinyBang.Test.Primitive.Int
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Utils.Language.Ast

-- TODO: write prop_testInteger using quickcheck

testInteger :: (?conf :: Cfg.Config) => Integer -> Test
testInteger i = lexParseEval (show i)
                             [flip TokIntegerLiteral i]
                             (astwrap $ A.PrimInt i)
                             (A.VPrimInt i :: A.Value IA.Expr)

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Integer tests" $ TestList $
  [ testInteger 1234567890
  , testInteger (-1234567890)
  ] ++ map testInteger [-50, 50] ++
  [ fPars "1_1"
  , xPars "_1" (astwrap $ A.Var $ ident "_1")
  , fPars "1_"
  , xLexs "0x1"
      [flip TokIntegerLiteral 0, flip TokIdentifier "x1"]
  , xLexs "-0"
      [flip TokIntegerLiteral 0]
  ]
