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

-- Helper function
interpret :: String -> EvalM
interpret = evalTop . parseBigBang . lexBigBang

-- Test cases that ensure that primitive literals are interpereted correctly
literalsCases :: Test
literalsCases = TestList [testInterpretInt, testInterpretChar]

testInterpretInt :: Test
testInterpretInt = TestCase $ assertEqual
  "Test if input 1234567890 is interpreted correctly"
  (Right (PrimInt 1234567890))
  (interpret "1234567890")

testInterpretChar :: Test
testInterpretChar = TestCase $ assertEqual
  "Test if input \'a\' interprets correctly"
  (Right (PrimChar 'a'))
  (interpret "\'a\'")

-- Test cases that check function application and evaluation works as expected
functionCases :: Test
functionCases = TestList [testFuncAppl, testLambdaAppl, testPlusInt, testMinusInt, testMinusNegInt, testFunctionNesting, testYCombinatorAppl]

testFuncAppl :: Test
testFuncAppl = TestCase $ assertEqual
  "Test if function definitions are applied correctly"
  (Right (PrimInt 4))
  (interpret "(fun x -> plus x x) 2")

testLambdaAppl :: Test
testLambdaAppl = TestCase $ assertEqual
  "Test if lambda functions are applied correctly"
  (Right (PrimInt 4))
  (interpret "(\\x -> plus x x) 2")

testPlusInt :: Test
testPlusInt = TestCase $ assertEqual
  "Test is function plus works correctly on integers"
  (Right (PrimInt 4))
  (interpret "plus 2 2")

testMinusInt :: Test
testMinusInt = TestCase $ assertEqual
  "Test if function minus works correctly on integers"
  (Right (PrimInt 0))
  (interpret "minus 2 2")

testMinusNegInt :: Test
testMinusNegInt = TestCase $ assertEqual
  "Test if minus works correctly when the second arg is a negative integer"
  (Right (PrimInt 4))
  (interpret "minus 2 -2")

testFunctionNesting :: Test
testFunctionNesting = TestCase $ assertEqual
  "Test if function nesting works correctly"
  (Right (PrimInt 4))
  (interpret "plus (minus 1 -1) (minus 1 -1)")

testYCombinatorAppl :: Test
testYCombinatorAppl = TestCase $ assertEqual
  "Test if recursive application using the y-combinator works correctly"
  (Right (PrimInt 15))
  (evalTop ast') 
  where
    yAst = parseBigBang $ lexBigBang "fun body -> (fun f -> fun arg -> f f arg) (fun this -> fun arg -> body (this this) arg)"
    ast = Appl yAst $ Func (ident "this") $ Func (ident "x") $ (Case (Equal (Var $ ident "x") (PrimInt 0)) [(Nothing, ChiLabel (labelName "True") (ident "z"), (PrimInt 0)), (Nothing, ChiLabel (labelName "False") (ident "z"), (Plus (Var $ ident "x") (Appl (Var $ ident "this") (Minus (Var $ ident "x") $ PrimInt 1))))])
    ast' = Appl ast $ PrimInt $ 5


-- Test cases that check functionality of Onions
onionCases :: Test
onionCases = TestList [testOnionIsomorphism, testOnionNotEqual, testOnionNotEqual2, testOnionNotEqual3, testCaseOnion, testSubOnionPlus, testOnionFuncApply]

testOnionIsomorphism :: Test
testOnionIsomorphism = TestCase $ do
  assertEqual
    "equal (1 & \'a\') (\'a\' & 1) should return true"
    (Right (Label (labelName "True") PrimUnit))
    (interpret "equal (1 & \'a\') (\'a\' & 1)")
  assertEqual
    "equal (\'a\' & 1) (1 & \'a\') should return true"
    (Right (Label (labelName "True") PrimUnit))
    (interpret "equal (\'a\' & 1) (1 & \'a\')")

testOnionNotEqual :: Test
testOnionNotEqual = TestCase $ assertEqual
  "equal (1 & \'a\') (1 & \'z\') should return false"
  (Right (Label (labelName "False") PrimUnit))
  (interpret "equal (1 & \'a\') (1 & \'z\')")

testOnionNotEqual2 :: Test
testOnionNotEqual2 = TestCase $ assertEqual
  "equal (1 & \'a\') (0 & \'a\') should return false"
  (Right (Label (labelName "False") PrimUnit))
  (interpret "equal (1 & \'a\') (0 & \'a\')")

testOnionNotEqual3 :: Test
testOnionNotEqual3 = TestCase $ assertEqual
  "equal (1 & 2) (2 & 1) should return false"
  (Right (Label (labelName "False") PrimUnit))
  (interpret "equal (1 & 2) (2 & 1)")

testCaseOnion :: Test
testCaseOnion = TestCase $ assertEqual
  "case `A 5 & `A \'a\' of {\n    A x -> x} should return 5 & \'a\'"
  (Right (Onion (PrimInt 5) (PrimChar 'a')))
  (interpret "case `A 5 & `A \'a\' of {\
                                       \    `A x -> x}")

testSubOnionPlus :: Test
testSubOnionPlus = TestCase $ assertEqual
  "plus (1 & \'a\') (\'a\' & 1 & ()) should return 2"
  (Right (PrimInt 2))
  (interpret "plus (1 & \'a\') (\'a\' & 1 & ())")

testOnionFuncApply :: Test
testOnionFuncApply = TestCase $ assertEqual
  "\"(1 & (fun x -> x)) 1\" should return 1"
  (Right (PrimInt 1))
  (interpret "(1 & (fun x -> x)) 1")

-- Test cases that check equality works as expected
equalCases :: Test
equalCases = TestList [testEqualInt, testEqualChar, testEqualBoolean, testEqualLabel, testEqualLabel2]

testEqualInt :: Test
testEqualInt = TestCase $ do
  assertEqual
    "equal 1 1 should return true"
    (Right (Label (labelName "True") PrimUnit))
    (interpret "equal 1 1")
  assertEqual
    "equal 0 1 should return false"
    (Right (Label (labelName "False") PrimUnit))
    (interpret "equal 0 1")
  assertEqual
    "equal 0 (minus (minus (plus 1 1) 1) 1) should return true"
    (Right (Label (labelName "True") PrimUnit))
    (interpret "equal 0 (minus (minus (plus 1 1) 1) 1)")

testEqualChar :: Test
testEqualChar = TestCase $ do
  assertEqual
    "equal \'a\' \'a\' should return true"
    (Right (Label (labelName "True") PrimUnit))
    (interpret "equal \'a\' \'a\'")
  assertEqual
    "equal \'a\' \'A\' should return false"
    (Right (Label (labelName "False") PrimUnit))
    (interpret "equal \'a\' \'A\'")

testEqualBoolean :: Test
testEqualBoolean = TestCase $ do
  assertEqual
    "equal `True () `True () should return true"
    (Right (Label (labelName "True") PrimUnit))
    (interpret "equal `True () `True ()")
  -- TODO: the following test case is broken; we should expect a dynamic type
  -- error
  assertEqual
    "equal `True () `False () should return false"
    (Right (Label (labelName "True") PrimUnit))
    (interpret "equal `True () `False ()")

testEqualLabel :: Test
testEqualLabel = TestCase $ assertEqual
  "equal `A 1 `A 1 should return true"
  (Right (Label (labelName "True") PrimUnit))
  (interpret "equal `A 1 `A 1")

testEqualLabel2 :: Test
testEqualLabel2 = TestCase $ assertEqual
  "equal `A 1 `B 1 should throw a type error"
  (Left (DynamicTypeError "incorrect type in expression"))
  (interpret "equal `A 1 `B 1")


-- Test cases that check type pattern matching in case...of blocks
caseCases :: Test
caseCases = TestList [testCaseChar, testCaseInt, testCaseFun, testCaseLambda, testCaseUnit, testCaseLabel, testCaseLabel2, testCaseLabel3, testCaseFirstApplicable]

testCaseChar :: Test
testCaseChar = TestCase $ assertEqual
  "Test if case matches characters correctly"
  (Right (PrimInt 0))
  (interpret "case 'a' of {\
                                       \    char -> 0}")

testCaseInt :: Test
testCaseInt = TestCase $ assertEqual
  "Test if case matches integers correctly"
  (Right (PrimInt 0))
  (interpret "case 1234567890 of {\
                                       \    int -> 0}")

testCaseFun :: Test
testCaseFun = TestCase $ assertEqual
  "Test if case matches functions correctly"
  (Right (PrimInt 0))
  (interpret "case (fun x -> x) of {\
                                       \    fun -> 0}")

testCaseLambda :: Test
testCaseLambda = TestCase $ assertEqual
  "Test if case matches lambdas correctly"
  (Right (PrimInt 0))
  (interpret "case (\\x -> x) of {\
                                       \    fun -> 0}")

testCaseUnit :: Test
testCaseUnit = TestCase $ assertEqual
  "Test if case matches units correctly"
  (Right (PrimInt 0))
  (interpret "case () of {\
                                       \    unit -> 0}")

testCaseLabel :: Test
testCaseLabel = TestCase $ assertEqual
  "Test if case matches labels correctly"
  (Right (PrimInt 0))
  (interpret "case `Test () of {\
                                       \    `Test a -> 0}")

testCaseLabel2 :: Test
testCaseLabel2 = TestCase $ assertEqual
  "Test if case matches correct label"
  (Right (PrimInt 1))
  (interpret "case `B 2 of {\
                                       \    `A x -> 0;\
                                       \    `B y -> 1}")

testCaseLabel3 :: Test
testCaseLabel3 = TestCase $ assertEqual
  "Test if case returns matched variable part of label"                
  (Right (PrimInt 0))
  (interpret "case `A 0 of {\
                                         \    `A n -> n}")

testCaseFirstApplicable :: Test
testCaseFirstApplicable = TestCase $ assertEqual
  "Test if casing matches the first applicable pattern"
  (Right (PrimInt 1))
  (interpret "case `A 1 of {\
                                         \    `A a -> 1;\
                                         \    `A n -> 0}")

-- Test cases that should return errors
errorCases :: Test
errorCases = TestList [testUnbound, testTypeMismatch, testAddFunction, testEqualTypeMismatch, testInvalidApplication, testNonexhaustiveCases, testEqualUnit]

testUnbound :: Test
testUnbound = TestCase $ assertEqual
  "Test if evaluation of free variable causes error"
  (Left (NotClosed (ident "x")))
  (interpret "x")

testTypeMismatch :: Test
testTypeMismatch = TestCase $ assertEqual
  "Test if type mismatch in function appl throws an error"
  (Left (DynamicTypeError "incorrect type in expression"))
  (interpret "plus 2 \'x\'")

testAddFunction :: Test
testAddFunction = TestCase $ assertEqual
  "Test if using a function definition in plus throws an error"
  (Left (DynamicTypeError "incorrect type in expression"))
  (interpret "plus 1 (fun x -> x)")

testEqualTypeMismatch :: Test
testEqualTypeMismatch = TestCase $ assertEqual
  "Checking for equality between values of different types should throw an error"
  (Left (DynamicTypeError "incorrect type in expression"))
  (interpret "equal 1 \'a\'")

testInvalidApplication :: Test
testInvalidApplication = TestCase $ assertEqual
  "Test if trying to apply something that is not a function throws an error"
  (Left (ApplNotFunction (PrimInt 1) (PrimChar 'x')))
  (interpret "1 \'x\'")

testNonexhaustiveCases :: Test
testNonexhaustiveCases = TestCase $ assertEqual
  "Test if nonexhaustive cases throws an error"
  (Left (UnmatchedCase (PrimInt 1) [(Nothing, ChiPrim T.PrimChar,PrimInt 0)])) 
  (interpret "case 1 of {\
                                       \    char -> 0}")

testEqualUnit :: Test
testEqualUnit = TestCase $ assertEqual
  "equal () () should return true"
  (Right (Label (labelName "True") PrimUnit))
  (interpret "equal () ()")
