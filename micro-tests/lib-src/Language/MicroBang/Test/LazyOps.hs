module Language.MicroBang.Test.LazyOps
( tests
) where

import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Test.ValueUtils as V
  ( pi
  )
import qualified Language.MicroBang.Test.ExpressionUtils as E
  ( pi
  )
import qualified Language.MicroBang.Ast as A
--import Language.MicroBang.Syntax.Lexer

-- TODO: write a quickcheck test that any valid +/- tree is correctly computed.

tests :: (?conf :: Bool) => Test
tests = TestLabel "Tests on integer operations" $ TestList
  [
  lexParseEval "[+] ([+] 3 1) 2"
                 (A.BinOp A.Plus (A.BinOp A.Plus (E.pi 3) (E.pi 1)) (E.pi 2))
                 (V.pi 6)
---- Test parse and evaluation of some simple arithmetic applications
  , xPars "[+] 2 2" $
          A.BinOp A.Plus (E.pi 2) (E.pi 2)
  , xEval "(fun x -> [+] x x) 2"
          (V.pi 4)
  , xEval "(fun x -> [+] x x) 2"
          (V.pi 4)
  , xEval "[+] 2 2"
          (V.pi 4)
  , xEval "[+] 2 -2"
          (V.pi 0)
---- Test that arithmetic expressions on non-numeric literals fail to typecheck
  , xCont "[+] 1 ()"
  , xCont "[+] () ()"
  , xCont "[+] 2  `A 3"
  , xCont "[+] 1 (fun x -> x)"
-- Test evaluation of compound arithmetic application
  , xEval "[+] ([+] 1 -1) ([+] 1 -1)"
          (V.pi 0)
  ]
