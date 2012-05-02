module Language.LittleBang.Test.LazyOps
( tests
) where

import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Test.ValueUtils as V
  ( pi
  )
import qualified Language.LittleBang.Test.ExpressionUtils as E
  ( pi
  )
import qualified Language.TinyBang.Ast as TA
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Config as Cfg
import Utils.Language.Ast
--import Language.LittleBang.Syntax.Lexer

-- TODO: write a quickcheck test that any valid +/- tree is correctly computed.

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests on integer operations" $ TestList
  [ lexParseEval "(3 - 1) + 2"
                 [ TokOpenParen
                 , TokIntegerLiteral 3
                 , TokOpMinus
                 , TokIntegerLiteral 1
                 , TokCloseParen
                 , TokOpPlus
                 , TokIntegerLiteral 2
                 ]
                 (astwrap $ TA.LazyOp TA.Plus
                    (astwrap $ TA.LazyOp TA.Minus (E.pi 3) (E.pi 1))
                    (E.pi 2))
                 (V.pi 4 :: TA.Value TA.Expr)
-- Test parse and evaluation of some simple arithmetic applications
  , xPars "2 + 2" $ astwrap $
          TA.LazyOp TA.Plus (E.pi 2) (E.pi 2)
  , xType "1 + 2"
  , xType "1 - 2"
  , xType "((1 + 2) - 3) + ((-2) + (4 - 0))"
  , xvEval "(fun x -> x + x) 2"
          (V.pi 4)
  , xvEval "2 + 2"
          (V.pi 4)
  , xvEval "2 - 2"
          (V.pi 0)
  , xvEval "2 - -2"
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
  , xvEval "(1 - -1) + (1 - -1)"
          (V.pi 4)
  ]
