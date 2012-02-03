module Language.TinyBang.Test.LazyOps
( tests
) where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Test.ValueUtils as V
  ( pi
  )
import qualified Language.TinyBang.Test.ExpressionUtils as E
  ( pi
  )
import qualified Language.TinyBang.Ast as A
--import Language.TinyBang.Syntax.Lexer

-- TODO: write a quickcheck test that any valid +/- tree is correctly computed.

tests :: (?debug :: Bool) => Test
tests = TestLabel "Tests on integer operations" $ TestList
  [ lexParseEval "[+] ([-] 3 1) 2"
                 [ TokOpPlus
                 , TokOpenParen
                 , TokOpMinus
                 , TokIntegerLiteral 3
                 , TokIntegerLiteral 1
                 , TokCloseParen
                 , TokIntegerLiteral 2
                 ]
                 (A.LazyOp A.Plus (A.LazyOp A.Minus (E.pi 3) (E.pi 1)) (E.pi 2))
                 (V.pi 4)
-- Test parse and evaluation of some simple arithmetic applications
  , xPars "[+] 2 2" $
          A.LazyOp A.Plus (E.pi 2) (E.pi 2)
  , xType "[+] 1 2"
  , xType "[-] 1 2"
  , xType "[+] ([-] ([+] 1 2) 3) ([+] (-2) ([-] 4 0))"
  , xEval "(fun x -> [+] x x) 2"
          (V.pi 4)
  , xEval "(fun x -> [+] x x) 2"
          (V.pi 4)
  , xEval "[+] 2 2"
          (V.pi 4)
  , xEval "[-] 2 2"
          (V.pi 0)
  , xEval "[-] 2 -2"
          (V.pi 4)
  , xType "(fun x -> [+] x 1) 1"
-- Test that arithmetic expressions on non-numeric literals fail to typecheck
  , xCont "[+] 1 'a'"
  , xCont "[+] 1 ()"
  , xCont "[+] 'a' 'a'"
  , xCont "[+] () ()"
  , xCont "[+] 2 'x'"
  , xCont "[+] 1 (fun x -> x)"
  , xCont "[-] 1 'a'"
  , xCont "[-] 1 ()"
  , xCont "[-] 'a' 'a'"
  , xCont "[-] () ()"
-- Test evaluation of compound arithmetic application
  , xEval "[+] ([-] 1 -1) ([-] 1 -1)"
          (V.pi 4)
  ]
