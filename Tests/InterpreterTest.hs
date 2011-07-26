module InterpreterTest where

import Test.HUnit hiding (Label)
import Language.BigBang.Ast
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Interpreter.Interpreter
import Language.BigBang.Syntax.Parser
import Language.BigBang.Syntax.Lexer

edgeCases = TestList []


simpleCases = TestList [testInterpretInt, testInterpretChar, testPlusInt, testLambdaAppl, testFuncAppl]

testInterpretInt = TestCase $ assertEqual
  "Test if input 1234567890 is interpreted correctly"
  (Right (PrimInt 1234567890))
  (evalTop $ parseBigBang $ lexBigBang "1234567890")

testInterpretChar = TestCase $ assertEqual
  "Test if input \'a\' interprets correctly"
  (Right (PrimChar 'a'))
  (evalTop $ parseBigBang $ lexBigBang "\'a\'")

testPlusInt = TestCase $ assertEqual
  "Test is function plus works correctly on integers"
  (Right (PrimInt 4))
  (evalTop $ parseBigBang $ lexBigBang "plus 2 2")

testLambdaAppl = TestCase $ assertEqual
  "Test if lambda functions are applied correctly"
  (Right (PrimInt 4))
  (evalTop $ parseBigBang $ lexBigBang "(\\x -> plus x x) 2")

testFuncAppl = TestCase $ assertEqual
  "Test if function definitions are applied correctly"
  (Right (PrimInt 4))
  (evalTop $ parseBigBang $ lexBigBang "(fun x -> plus x x) 2")

tests = TestList [edgeCases, simpleCases]
main = runTestTT tests
