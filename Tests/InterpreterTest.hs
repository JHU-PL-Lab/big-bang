module InterpreterTest where

import Test.HUnit hiding (Label)
import Language.BigBang.Ast
import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Interpreter.Interpreter
import Language.BigBang.Syntax.Parser
import Language.BigBang.Syntax.Lexer

edgeCases = TestList [testNonexhaustiveCases, testUnbound, testTypeMismatch, testInvalidApplication]

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

testNonexhaustiveCases = TestCase $ assertEqual
  "Test if nonexhaustive cases throws an error"
  (Left (UnmatchedCase (PrimInt 1) [(ChiPrim T.PrimChar,PrimInt 0)])) 
  (evalTop $ parseBigBang $ lexBigBang "case 1 of {\nchar -> 0}")

simpleCases = TestList [testCaseLabel, testCaseOnion, testCaseChar, testCaseInt, testCaseFun, testCaseLambda, testCaseUnit, testInterpretInt, testInterpretChar, testPlusInt, testLambdaAppl, testFuncAppl]

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

testCaseChar = TestCase $ assertEqual
  "Test if case matches characters correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case 'a' of {\nchar -> 0}")

testCaseInt = TestCase $ assertEqual
  "Test if case matches integers correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case 1234567890 of {\nint -> 0}")

testCaseFun = TestCase $ assertEqual
  "Test if case matches functions correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case (fun x -> x) of {\nfun -> 0}")

testCaseLambda = TestCase $ assertEqual
  "Test if case matches lambdas correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case (\\x -> x) of {\nfun -> 0}")

testCaseUnit = TestCase $ assertEqual
  "Test if case matches units correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case () of {\nunit -> 0}")

testCaseOnion = TestCase $ assertEqual
  "Test if case matches onions correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case 1&1  of {\na&b -> 0}")

testCaseLabel = TestCase $ assertEqual
  "Test if case matches labels correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case `Test () of {\n`Test a -> 0}")

tests = TestList [edgeCases, simpleCases]
main = runTestTT tests
