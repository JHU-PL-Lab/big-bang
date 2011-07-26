module InterpreterTest where

import Test.HUnit hiding (Label)
import Language.BigBang.Ast
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Interpreter.Interpreter
import Language.BigBang.Syntax.Parser
import Language.BigBang.Syntax.Lexer

edgeCases = TestList [testUnbound, testTypeMismatch, testInvalidApplication]

testUnbound = TestCase $ assertEqual
  "Test if evaluation of free variable causes error"
  (Left (NotClosed (ident "x")))
  (evalTop $ parseBigBang $ lexBigBang "x")

testTypeMismatch = TestCase $ assertEqual
  "Test if type mismatch in function appl throws an error"
  (Left (DynamicTypeError "incorrect type in expression"))
  (evalTop $ parseBigBang $ lexBigBang "plus 2 \'x\'")

testInvalidApplication = TestCase $ assertEqual
  "Test if trying to apply something that is not a function throws an error"
  (Left (ApplNotFunction (PrimInt 1) (PrimChar 'x')))
  (evalTop $ parseBigBang $ lexBigBang "1 \'x\'")

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
