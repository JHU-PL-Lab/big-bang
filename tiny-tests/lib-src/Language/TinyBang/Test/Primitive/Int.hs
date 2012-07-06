module Language.TinyBang.Test.Primitive.Int
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Data.ExtensibleVariant

-- TODO: write prop_testInteger using quickcheck

testInteger :: (?conf :: Cfg.Config) => Integer -> Test
testInteger i = lexParseEval (show i)
                             [TokIntegerLiteral i]
                             (inj $ A.PrimInt i)
                             (A.VPrimInt i :: A.Value IA.Expr)

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Integer tests" $ TestList $
  [ testInteger 1234567890
  , testInteger (-1234567890)
  ] ++ map testInteger [-50, 50] ++
  [ fLexs "1_1"
--    (inj $ A.Appl (inj $ A.PrimInt 1) (inj $ A.Var $ ident "_1"))
  , xPars "_1" (inj $ A.Var $ ident "_1")
  , fLexs "1_"
--    (inj $ A.Appl (inj $ A.PrimInt 1) (inj $ A.Var $ ident "_"))
  , fLexs "0x1"
--      [TokIntegerLiteral 0, TokIdentifier "x1"]
  , xLexs "-0"
      [TokIntegerLiteral 0]
  ]