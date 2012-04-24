module Language.LittleBang.Test.LazyOps
( tests
) where

import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Test.ValueUtils as V
import qualified Language.TinyBang.Config as Cfg
--import Language.LittleBang.Syntax.Lexer

-- TODO: write a quickcheck test that any valid +/- tree is correctly computed.

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests on integer operations" $ TestList
  [
-- Test parse and evaluation of some simple arithmetic applications
    xType "1 + 2"
  , xType "1 - 2"
  , xType "((1 + 2) - 3) + ((-2) + (4 - 0))"
  , xEval "(fun x -> x + x) 2"
          (V.pi 4)
  , xEval "2 + 2"
          (V.pi 4)
  , xEval "2 - 2"
          (V.pi 0)
  , xEval "2 - -2"
          (V.pi 4)
  , xType "(fun x -> x + 1) 1"
-- Test that arithmetic expressions on non-numeric literals fail to typecheck
  , xCont "1 + 'a'"
  , xCont "1 + ()"
  , xCont "'a' + 'a'"
  , xCont "() + ()"
  , xCont "2 + 'x'"
  , xCont "1 + (fun x -> x)"
  , xCont "1 + 'a'"
  , xCont "1 + ()"
  , xCont "'a' + 'a'"
  , xCont "() + ()"
-- Test evaluation of compound arithmetic application
  , xEval "(1 - -1) + (1 - -1)"
          (V.pi 4)
  ]
