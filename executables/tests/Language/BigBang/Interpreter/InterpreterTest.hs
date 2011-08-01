module Language.BigBang.Interpreter.InterpreterTest
( tests
) where

import Test.HUnit hiding (Label)
import Language.BigBang.Ast
import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Interpreter.Interpreter
import Language.BigBang.Syntax.Parser
import Language.BigBang.Syntax.Lexer

tests :: Test
tests = TestList [literalsCases, functionCases, onionCases, equalCases, caseCases, errorCases]

-- Test cases that ensure that primitive literals are interpereted correctly
literalsCases :: Test
literalsCases = TestList [testInterpretPositiveInt, testInterpretNegativeInt, testInterpretChar]

testInterpretPositiveInt :: Test
testInterpretPositiveInt = TestCase $ assertEqual
  "Test if input 1234567890 is interpreted correctly"
  (Right (PrimInt 1234567890))
  (evalTop $ parseBigBang $ lexBigBang "1234567890")

testInterpretNegativeInt :: Test
testInterpretNegativeInt = TestCase $ assertEqual
  "Test if input -1234567890 is interpreted correctly"
  (Right (PrimInt (-1234567890)))
  (evalTop $ parseBigBang $ lexBigBang "-1234567890")

testInterpretChar :: Test
testInterpretChar = TestCase $ assertEqual
  "Test if input \'a\' interprets correctly"
  (Right (PrimChar 'a'))
  (evalTop $ parseBigBang $ lexBigBang "\'a\'")

-- Test cases that check function application and evaluation works as expected
functionCases :: Test
functionCases = TestList [testFuncAppl, testLambdaAppl, testPlusInt, testMinusInt, testPlusNegInt, testMinusNegInt, testFunctionNesting]

testFuncAppl :: Test
testFuncAppl = TestCase $ assertEqual
  "Test if function definitions are applied correctly"
  (Right (PrimInt 4))
  (evalTop $ parseBigBang $ lexBigBang "(fun x -> plus x x) 2")

testLambdaAppl :: Test
testLambdaAppl = TestCase $ assertEqual
  "Test if lambda functions are applied correctly"
  (Right (PrimInt 4))
  (evalTop $ parseBigBang $ lexBigBang "(\\x -> plus x x) 2")

testPlusInt :: Test
testPlusInt = TestCase $ assertEqual
  "Test is function plus works correctly on integers"
  (Right (PrimInt 4))
  (evalTop $ parseBigBang $ lexBigBang "plus 2 2")

testMinusInt :: Test
testMinusInt = TestCase $ assertEqual
  "Test if function minus works correctly on integers"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "minus 2 2")

testPlusNegInt :: Test
testPlusNegInt = TestCase $ assertEqual
  "Test if plus works correctly when the secong arg is a negative integer"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "plus 2 -2")

testMinusNegInt :: Test
testMinusNegInt = TestCase $ assertEqual
  "Test if minus works correctly when the second arg is a negative integer"
  (Right (PrimInt 4))
  (evalTop $ parseBigBang $ lexBigBang "minus 2 -2")

testFunctionNesting :: Test
testFunctionNesting = TestCase $ assertEqual
  "Test if function nesting works correctly"
  (Right (PrimInt 4))
  (evalTop $ parseBigBang $ lexBigBang "plus (minus 1 -1) (minus 1 -1)")


-- Test cases that check functionality of Onions
onionCases :: Test
onionCases = TestList [testOnionIsomorphism, testOnionNotEqual, testOnionNotEqual2, testOnionNotEqual3]

testOnionIsomorphism :: Test
testOnionIsomorphism = TestCase $ do
  assertEqual
    "equal (1 & \'a\') (\'a\' & 1) should return true"
    (Right (Label (labelName "True") PrimUnit))
    (evalTop $ parseBigBang $ lexBigBang "equal (1 & \'a\') (\'a\' & 1)")
  assertEqual
    "equal (\'a\' & 1) (1 & \'a\') should return true"
    (Right (Label (labelName "True") PrimUnit))
    (evalTop $ parseBigBang $ lexBigBang "equal (\'a\' & 1) (1 & \'a\')")

testOnionNotEqual :: Test
testOnionNotEqual = TestCase $ assertEqual
  "equal (1 & \'a\') (1 & \'z\') should return false"
  (Right (Label (labelName "False") PrimUnit))
  (evalTop $ parseBigBang $ lexBigBang "equal (1 & \'a\') (1 & \'z\')")

testOnionNotEqual2 :: Test
testOnionNotEqual2 = TestCase $ assertEqual
  "equal (1 & \'a\') (0 & \'a\') should return false"
  (Right (Label (labelName "False") PrimUnit))
  (evalTop $ parseBigBang $ lexBigBang "equal (1 & \'a\') (0 & \'a\')")

testOnionNotEqual3 :: Test
testOnionNotEqual3 = TestCase $ assertEqual
  "equal (1 & 2) (2 & 1) should return false"
  (Right (Label (labelName "False") PrimUnit))
  (evalTop $ parseBigBang $ lexBigBang "equal (1 & 2) (2 & 1)")


-- Test cases that check equality works as expected
equalCases :: Test
equalCases = TestList [testEqualInt, testEqualChar, testEqualBoolean, testEqualLabel, testEqualLabel2]

testEqualInt :: Test
testEqualInt = TestCase $ do
  assertEqual
    "equal 1 1 should return true"
    (Right (Label (labelName "True") PrimUnit))
    (evalTop $ parseBigBang $ lexBigBang "equal 1 1")
  assertEqual
    "equal 0 1 should return false"
    (Right (Label (labelName "False") PrimUnit))
    (evalTop $ parseBigBang $ lexBigBang "equal 0 1")
  assertEqual
    "equal 0 (minus (minus (plus 1 1) 1) 1) should return true"
    (Right (Label (labelName "True") PrimUnit))
    (evalTop $ parseBigBang $ lexBigBang "equal 0 (minus (minus (plus 1 1) 1) 1)")

testEqualChar :: Test
testEqualChar = TestCase $ do
  assertEqual
    "equal \'a\' \'a\' should return true"
    (Right (Label (labelName "True") PrimUnit))
    (evalTop $ parseBigBang $ lexBigBang "equal \'a\' \'a\'")
  assertEqual
    "equal \'a\' \'A\' should return false"
    (Right (Label (labelName "False") PrimUnit))
    (evalTop $ parseBigBang $ lexBigBang "equal \'a\' \'A\'")

testEqualBoolean :: Test
testEqualBoolean = TestCase $ do
  assertEqual
    "equal `True () `True () should return true"
    (Right (Label (labelName "True") PrimUnit))
    (evalTop $ parseBigBang $ lexBigBang "equal `True () `True ()")
  assertEqual
    "equal `True () `False () should return false"
    (Right (Label (labelName "True") PrimUnit))
    (evalTop $ parseBigBang $ lexBigBang "equal `True () `False ()")

testEqualLabel :: Test
testEqualLabel = TestCase $ assertEqual
  "equal `A 1 `A 1 should return true"
  (Right (Label (labelName "True") PrimUnit))
  (evalTop $ parseBigBang $ lexBigBang "equal `A 1 `A 1")

testEqualLabel2 :: Test
testEqualLabel2 = TestCase $ assertEqual
  "equal `A 1 `B 1 should throw a type error"
  (Left (DynamicTypeError "incorrect type in expression"))
  (evalTop $ parseBigBang $ lexBigBang "equal `A 1 `B 1")


-- Test cases that check type pattern matching in case...of blocks
caseCases :: Test
caseCases = TestList [testCaseChar, testCaseInt, testCaseFun, testCaseLambda, testCaseUnit, testCaseLabel]

testCaseChar :: Test
testCaseChar = TestCase $ assertEqual
  "Test if case matches characters correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case 'a' of {\nchar -> 0}")

testCaseInt :: Test
testCaseInt = TestCase $ assertEqual
  "Test if case matches integers correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case 1234567890 of {\nint -> 0}")

testCaseFun :: Test
testCaseFun = TestCase $ assertEqual
  "Test if case matches functions correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case (fun x -> x) of {\nfun -> 0}")

testCaseLambda :: Test
testCaseLambda = TestCase $ assertEqual
  "Test if case matches lambdas correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case (\\x -> x) of {\nfun -> 0}")

testCaseUnit :: Test
testCaseUnit = TestCase $ assertEqual
  "Test if case matches units correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case () of {\nunit -> 0}")

testCaseLabel :: Test
testCaseLabel = TestCase $ assertEqual
  "Test if case matches labels correctly"
  (Right (PrimInt 0))
  (evalTop $ parseBigBang $ lexBigBang "case `Test () of {\n`Test a -> 0}")


-- Test cases that should return errors
errorCases :: Test
errorCases = TestList [testUnbound, testTypeMismatch, testEqualTypeMismatch, testInvalidApplication, testNonexhaustiveCases, testEqualUnit]
testUnbound :: Test
testUnbound = TestCase $ assertEqual
  "Test if evaluation of free variable causes error"
  (Left (NotClosed (ident "x")))
  (evalTop $ parseBigBang $ lexBigBang "x")

testTypeMismatch :: Test
testTypeMismatch = TestCase $ assertEqual
  "Test if type mismatch in function appl throws an error"
  (Left (DynamicTypeError "incorrect type in expression"))
  (evalTop $ parseBigBang $ lexBigBang "plus 2 \'x\'")

testEqualTypeMismatch :: Test
testEqualTypeMismatch = TestCase $ assertEqual
  "Checking for equality between values of different types should throw an error"
  (Left (DynamicTypeError "incorrect type in expression"))
  (evalTop $ parseBigBang $ lexBigBang "equal 1 \'a\'")

testInvalidApplication :: Test
testInvalidApplication = TestCase $ assertEqual
  "Test if trying to apply something that is not a function throws an error"
  (Left (ApplNotFunction (PrimInt 1) (PrimChar 'x')))
  (evalTop $ parseBigBang $ lexBigBang "1 \'x\'")

testNonexhaustiveCases :: Test
testNonexhaustiveCases = TestCase $ assertEqual
  "Test if nonexhaustive cases throws an error"
  (Left (UnmatchedCase (PrimInt 1) [(ChiPrim T.PrimChar,PrimInt 0)])) 
  (evalTop $ parseBigBang $ lexBigBang "case 1 of {\nchar -> 0}")

testEqualUnit :: Test
testEqualUnit = TestCase $ assertEqual
  "equal () () should throw a type error"
  (Left (DynamicTypeError "incorrect type in expression"))
  (evalTop $ parseBigBang $ lexBigBang "equal () ()")
