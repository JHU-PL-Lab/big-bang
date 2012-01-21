module Language.TinyBang.Render.PrettyPrintTest
( tests
) where

import Test.HUnit hiding (Label)
import Language.TinyBang.Ast
import Language.TinyBang.Types.UtilTypes
import Language.TinyBang.Render.Display

tests :: Test
tests = TestList [printPrimCases, printFuncTests, printOnionTests{-, printOpsTests-}]

-- Test cases that ensure that primitive literals are printed correctly
printPrimCases :: Test
printPrimCases = TestList [testPrintPositiveInt, testPrintNegativeInt, testPrintChar, testPrintVar, testPrintUnit, testPrintBoolean]

testPrintPositiveInt :: Test
testPrintPositiveInt = TestCase $ assertEqual
  "Test if input 1234567890 is printed correctly"
  "1234567890"
  (display (PrimInt 1234567890))

testPrintNegativeInt :: Test
testPrintNegativeInt = TestCase $ assertEqual
  "Test if input -1234567890 is printed correctly"
  "-1234567890"
  (display (PrimInt (-1234567890)))

testPrintChar :: Test
testPrintChar = TestCase $ assertEqual
  "Test if input \'a\' prints correctly"
  "'a'"
  (display (PrimChar 'a'))

testPrintVar :: Test
testPrintVar = TestCase $ assertEqual
  "Test if variables are printed correctly"
  "x"
  (display (Var (ident "x")))

testPrintUnit :: Test
testPrintUnit = TestCase $ assertEqual
  "Test if unit prints correctly"
  "()"
  (display PrimUnit)

testPrintBoolean :: Test
testPrintBoolean = TestCase $ do
  assertEqual
    "Test is boolean True prints correctly"
    "`True ()"
    (display (Label (labelName "True") PrimUnit))
  assertEqual
    "Test is boolean False prints correctly"
    "`False ()"
    (display (Label (labelName "False") PrimUnit))

-- Test cases that check pretty printing of function definitions and applications
printFuncTests :: Test
printFuncTests = TestList [testPrintFunction1, testPrintFunction2, testPrintFuncAppl1, testPrintFuncAppl2, testPrintPerverse, testPrintFunction3]

testPrintFunction1 :: Test
testPrintFunction1 = TestCase $ assertEqual
  "Test if I combinator is printed correctly"
  "(fun x -> x)"
  (display (Func (ident "x") (Var (ident "x"))))

testPrintFunction2 :: Test
testPrintFunction2 = TestCase $ assertEqual
  "Test if K combinator is printed correctly"
  "(fun x -> (fun y -> x))"
  (display (Func (ident "x") (Func (ident "y") (Var (ident "x")))))

testPrintFuncAppl1 :: Test
testPrintFuncAppl1 = TestCase $ assertEqual
  "Test if function application is printed correctly"
  "plus 2 2"
  (display (Appl (Appl (Var (ident "plus")) (PrimInt 2)) (PrimInt 2)))

testPrintFuncAppl2 :: Test
testPrintFuncAppl2 = TestCase $ assertEqual
  "Test if printing function with variables and characters displays correctly"
  "plus x \'x\'"
  (display (Appl (Appl (Var (ident "plus")) (Var (ident "x"))) (PrimChar 'x')))


testPrintPerverse :: Test
testPrintPerverse = TestCase $ assertEqual
  "Test if perverse function aprettylication is printed correctly"
  "(fun x -> x x) (fun x -> x x)"
  (display (Appl (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x")))) (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x"))))))

testPrintFunction3 :: Test
testPrintFunction3 = TestCase $ assertEqual
  "Test if S combinator is pretty printed correctly"
  "(fun x -> (fun y -> (fun z -> x z y z)))"
  (display (Func (ident "x") (Func (ident "y") (Func (ident "z") (Appl (Appl (Var (ident "x")) (Var (ident "z"))) (Appl (Var (ident "y")) (Var (ident "z"))))))))


-- Test cases that check pretty printing of onions
printOnionTests :: Test
printOnionTests = TestList [testPrintOnion, testPrintTernaryOnion]

testPrintOnion :: Test
testPrintOnion = TestCase $ assertEqual
  "Test if onion prints correctly"
  "1 & 2"
  (display (Onion (PrimInt 1) (PrimInt 2)))

testPrintTernaryOnion :: Test
testPrintTernaryOnion = TestCase $ assertEqual
  "Test if ternary onion prints correctly"
  "1 & 2 & 3"
  (display (Onion (PrimInt 1) (Onion (PrimInt 2) (PrimInt 3))))


-- Test cases that check pretty printing of basic builtin operators
-- printOpsTests :: Test
-- printOpsTests = TestList [testPrintPlus, testPrintMinus, testPrintEqual]

-- testPrintPlus :: Test
-- testPrintPlus = TestCase $ assertEqual
--   "Test that primitive add prints correctly"
--   "1 [+] 1"
--   (display (Plus (PrimInt 1) (PrimInt 1)))

-- testPrintMinus :: Test
-- testPrintMinus = TestCase $ assertEqual
--   "Test that primitive subtract prints correctly"
--   "1 [-] 1"
--   (display (Minus (PrimInt 1) (PrimInt 1)))

-- testPrintEqual :: Test
-- testPrintEqual = TestCase $ assertEqual
--   "Test that primitive equals prints correctly"
--   "1 [=] 1"
--   (display (Equal (PrimInt 1) (PrimInt 1)))