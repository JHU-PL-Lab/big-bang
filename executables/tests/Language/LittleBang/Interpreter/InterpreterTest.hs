module Language.LittleBang.Interpreter.InterpreterTest
( tests
) where

import Test.HUnit hiding (Label)
import Language.LittleBang.Ast
import qualified Language.LittleBang.Types.Types as T
import qualified Language.LittleBang.Types.TypeInference as TI
import Language.LittleBang.Types.UtilTypes
import Language.LittleBang.Interpreter.Interpreter
import Language.LittleBang.Render.Display
import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Language.LittleBang.Interpreter.SourceInterpreter

import Debug.Trace

type LittleBangCode = String

xEval :: LittleBangCode -> Expr -> Test
xEval code expectedResult =
  label ~: case wrappedResult of
    EvalSuccess _ result -> expectedResult ~=? result
    _ -> TestCase $ assertFailure $
         "Expected evaluation to succeed but instead got " ++
         display wrappedResult
  where wrappedResult = evalStringTop code
        label = show code ++
                " was expected to produce " ++ display expectedResult

assertSuccess :: Assertion
assertSuccess = return ()

xCont :: LittleBangCode -> Test
xCont code =
  label ~: case result of
    Contradiction _ _ -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expression evaluated to " ++
         display result ++
         ", which did not produce a contradiction"
  where wrappedResult = evalStringTop code
        label = show code ++
                " was expected to produce a contradiction"
        result = evalStringTop code

xNotC :: LittleBangCode -> Test
xNotC code =
  label ~: case result of
    TypecheckFailure _ (TI.NotClosed _) _ -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expected NotClosed but instead got " ++
         display result
  where wrappedResult = evalStringTop code
        label = show code ++
                " was expected to produce a contradiction"
        result = evalStringTop code

srcY = "fun body -> (fun f -> fun arg -> f f arg) (fun this -> fun arg -> body (this this) arg)"
srcSummate = "fun this -> fun x -> case (equal x 0) of { `True z -> 0 ; `False z -> plus x (this (minus x 1))}"
srcMultiAppl [] = error "srcMultiAppl used on empty list"
srcMultiAppl xs = concatMap (\x -> "(" ++ x ++ ")") xs
true = (Label (labelName "True") PrimUnit)
false = (Label (labelName "False") PrimUnit)

tests = TestList
  [ xEval "1234567890"
          (PrimInt 1234567890)
  , xEval "'a'"
          (PrimChar 'a')
  , xEval "(fun x -> plus x x) 2"
          (PrimInt 4)
  , xEval "(\\x -> plus x x) 2"
          (PrimInt 4)
  , xEval "plus 2 2"
          (PrimInt 4)
  , xEval "minus 2 2"
          (PrimInt 0)
  , xEval "minus 2 -2"
          (PrimInt 4)
  , xEval "plus (minus 1 -1) (minus 1 -1)"
          (PrimInt 4)
  , xEval (srcMultiAppl [srcY, srcSummate, "5"])
          (PrimInt 15)
  , xEval "equal (1 & 'a') ('a' & 1)"
          true
  , xEval "equal ('a' & 1) (1 & 'a')"
          true
  , xEval "equal (1 & 'a') (1 & 'z')"
          false
  , xEval "equal (1 & 'a') (0 & 'a')"
          false
  , xEval "equal (1 & 2) (2 & 1)"
          false
  , xEval "case `A 5 & `A \'a\' of {`A x -> x}"
          (Onion (PrimInt 5) (PrimChar 'a'))
  , xEval "plus (1 & 'a') ('a' & 1 & ())"
          (PrimInt 2)
  , xEval "(1 & (fun x -> x)) 1"
          (PrimInt 1)
  , xEval "equal 1 1"
          true
  , xEval "equal 0 1"
          false
  , xEval "equal 0 (minus (minus (plus 1 1) 1) 1)"
          true
  , xEval "equal 'a' 'a'"
          true
  , xEval "equal 'a' 'A'"
          false
  , xEval "equal `True () `True ()"
          true
  , xCont "equal `True () `False ()"
  , xEval "equal `A 1 `A 1"
          true
  , xCont "equal `A 1 `B 1"
  , xEval "case 'a' of {char -> 0}"
          (PrimInt 0)
  , xEval "case 1234567890 of {int -> 0}"
          (PrimInt 0)
  , xEval "case (fun x -> x) of {fun -> 0}"
          (PrimInt 0)
  , xEval "case (\\x -> x) of {fun -> 0}"
          (PrimInt 0)
  , xEval "case () of {unit -> 0}"
          (PrimInt 0)
  , xEval "case `Test () of {`Test a -> 0}"
          (PrimInt 0)
  , xEval "case `B 2 of {`A x -> 0; `B y -> 1}"
          (PrimInt 1)
  , xEval "case `A 0 of {`A n -> n}"
          (PrimInt 0)
  , xEval "case `A 1 of {`A a -> 1; `A n -> 0}"
          (PrimInt 1)
  , xNotC "x"
  , xEval "equal () ()"
          true
  , xCont "plus 2 'x'"
  , xCont "plus 1 (fun x -> x)"
  , xCont "equal 1 'a'"
  , xCont "1 'x'"
  , xCont "case 1 of {char -> 0}"
  ]



