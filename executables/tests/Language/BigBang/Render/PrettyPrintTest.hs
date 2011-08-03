module Language.BigBang.Render.PrettyPrintTest
( tests
) where

import Test.HUnit hiding (Label)
import Language.BigBang.Ast
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Render.PrettyPrint

tests :: Test
tests = TestList [printPrimCases, printFuncTests, printOnionTests, printOpsTests]

-- Test cases that ensure that primitive literals are printed correctly
printPrimCases :: Test
printPrimCases = TestList [testPrintPositiveInt, testPrintNegativeInt, testPrintChar, testPrintVar, testPrintUnit, testPrintBoolean]

testPrintPositiveInt :: Test
testPrintPositiveInt = TestCase $ assertEqual
  "Test if input 1234567890 is printed correctly"
  "1234567890"
  (pretty (PrimInt 1234567890))

testPrintNegativeInt :: Test
testPrintNegativeInt = TestCase $ assertEqual
  "Test if input -1234567890 is printed correctly"
  "-1234567890" 
  (pretty (PrimInt (-1234567890)))

testPrintChar :: Test
testPrintChar = TestCase $ assertEqual
  "Test if input \'a\' prints correctly"
  "'a'"
  (pretty (PrimChar 'a'))

testPrintVar :: Test
testPrintVar = TestCase $ assertEqual
  "Test if variables are printed correctly"
  "x"
  (pretty (Var (ident "x")))

testPrintUnit :: Test
testPrintUnit = TestCase $ assertEqual
  "Test if unit prints correctly"
  "()"
  (pretty PrimUnit)

testPrintBoolean :: Test
testPrintBoolean = TestCase $ do
  assertEqual
    "Test is boolean True prints correctly"
    "`True ()"
    (pretty (Label (labelName "True") PrimUnit))
  assertEqual
    "Test is boolean False prints correctly"
    "`False ()"
    (pretty (Label (labelName "False") PrimUnit))

-- Test cases that check pretty printing of function definitions and applications
printFuncTests :: Test
printFuncTests = TestList [testPrintFunction, testPrintFuncAppl, testPrintPerverse]

testPrintFunction :: Test
testPrintFunction = TestCase $ assertEqual
  "Test if function is printed correctly"
  "(fun x -> x)"
  (pretty (Func (ident "x") (Var (ident "x"))))

testPrintFuncAppl :: Test
testPrintFuncAppl = TestCase $ assertEqual
  "Test if function aprettylication is printed correctly"
  "plus 2 2"
  (pretty (Appl (Appl (Var (ident "plus")) (PrimInt 2)) (PrimInt 2)))

testPrintPerverse :: Test
testPrintPerverse = TestCase $ assertEqual
  "Test if perverse function aprettylication is printed correctly"
  "(fun x -> x x) (fun x -> x x)"
  (pretty (Appl (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x")))) (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x"))))))


-- Test cases that check pretty printing of onions
printOnionTests :: Test
printOnionTests = TestList [testPrintOnion, testPrintTernaryOnion] 

testPrintOnion :: Test
testPrintOnion = TestCase $ assertEqual
  "Test if onion prints correctly"
  "1 & 2"
  (pretty (Onion (PrimInt 1) (PrimInt 2)))

testPrintTernaryOnion :: Test
testPrintTernaryOnion = TestCase $ assertEqual
  "Test if ternary onion prints correctly"
  "1 & 2 & 3"
  (pretty (Onion (PrimInt 1) (Onion (PrimInt 2) (PrimInt 3))))


-- Test cases that check pretty printing of basic builtin operators
printOpsTests :: Test
printOpsTests = TestList [testPrintPlus, testPrintMinus, testPrintEqual]

testPrintPlus :: Test
testPrintPlus = TestCase $ assertEqual
  "Test that primitive add prints correctly"
  "1 [+] 1"
  (pretty (Plus (PrimInt 1) (PrimInt 1)))

testPrintMinus :: Test
testPrintMinus = TestCase $ assertEqual
  "Test that primitive subtract prints correctly"
  "1 [-] 1"
  (pretty (Minus (PrimInt 1) (PrimInt 1)))

testPrintEqual :: Test
testPrintEqual = TestCase $ assertEqual
  "Test that primitive equals prints correctly"
  "1 [=] 1"
  (pretty (Equal (PrimInt 1) (PrimInt 1)))
