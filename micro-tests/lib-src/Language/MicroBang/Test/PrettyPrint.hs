module Language.MicroBang.Test.PrettyPrint
( tests
) where

import Test.HUnit hiding (Label)
import Language.MicroBang.Ast
import Language.MicroBang.Types.UtilTypes hiding (PrimitiveType(..))
import Utils.Render.Display

-- TODO: Refactor this or eliminate it.
-- TODO: Use quickcheck to write a test that pretty print + parse is idempotent.

tests :: (?debug :: Bool) => Test
tests = TestList [printPrimCases, printFuncTests, printOnionTests{-, printOpsTests-}]

-- Test cases that ensure that primitive literals are printed correctly
printPrimCases :: (?debug :: Bool) => Test
printPrimCases = TestList [testPrintPositiveInt, testPrintNegativeInt, testPrintVar, testPrintUnit, testPrintBoolean]

testPrintPositiveInt :: (?debug :: Bool) => Test
testPrintPositiveInt = TestCase $ assertEqual
  "Test if input 1234567890 is printed correctly"
  "1234567890"
  (display (PrimInt 1234567890))

testPrintNegativeInt :: (?debug :: Bool) => Test
testPrintNegativeInt = TestCase $ assertEqual
  "Test if input -1234567890 is printed correctly"
  "-1234567890"
  (display (PrimInt (-1234567890)))

-- MicroBang does not have chars
--testPrintChar :: (?debug :: Bool) => Test
--testPrintChar = TestCase $ assertEqual
--  "Test if input \'a\' prints correctly"
--  "'a'"
--  (display (PrimChar 'a'))

testPrintVar :: (?debug :: Bool) => Test
testPrintVar = TestCase $ assertEqual
  "Test if variables are printed correctly"
  "x"
  (display (Var (ident "x")))

testPrintUnit :: (?debug :: Bool) => Test
testPrintUnit = TestCase $ assertEqual
  "Test if unit prints correctly"
  "()"
  (display PrimUnit)

testPrintBoolean :: (?debug :: Bool) => Test
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
printFuncTests :: (?debug :: Bool) => Test
printFuncTests = TestList [testPrintFunction1, testPrintFunction2, testPrintFuncAppl1, testPrintFuncAppl2, testPrintPerverse, testPrintFunction3]

testPrintFunction1 :: (?debug :: Bool) => Test
testPrintFunction1 = TestCase $ assertEqual
  "Test if I combinator is printed correctly"
  "(fun x -> x)"
  (display (Func (ident "x") (Var (ident "x"))))

testPrintFunction2 :: (?debug :: Bool) => Test
testPrintFunction2 = TestCase $ assertEqual
  "Test if K combinator is printed correctly"
  "(fun x -> (fun y -> x))"
  (display (Func (ident "x") (Func (ident "y") (Var (ident "x")))))

testPrintFuncAppl1 :: (?debug :: Bool) => Test
testPrintFuncAppl1 = TestCase $ assertEqual
  "Test if function application is printed correctly"
  "((plus 2) 2)"
  (display (Appl (Appl (Var (ident "plus")) (PrimInt 2)) (PrimInt 2)))

testPrintFuncAppl2 :: (?debug :: Bool) => Test
testPrintFuncAppl2 = TestCase $ assertEqual
  "Test if printing function with variables and ints displays correctly"
  "((plus x) \'x\')"
  (display (Appl (Appl (Var (ident "plus")) (Var (ident "x"))) (PrimInt 5)))


testPrintPerverse :: (?debug :: Bool) => Test
testPrintPerverse = TestCase $ assertEqual
  "Test if perverse function application is printed correctly"
  "((fun x -> (x x)) (fun x -> (x x)))"
  (display (Appl (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x")))) (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x"))))))

testPrintFunction3 :: (?debug :: Bool) => Test
testPrintFunction3 = TestCase $ assertEqual
  "Test if S combinator is pretty printed correctly"
  "(fun x -> (fun y -> (fun z -> ((x z) (y z)))))"
  (display (Func (ident "x") (Func (ident "y") (Func (ident "z") (Appl (Appl (Var (ident "x")) (Var (ident "z"))) (Appl (Var (ident "y")) (Var (ident "z"))))))))


-- Test cases that check pretty printing of onions
printOnionTests :: (?debug :: Bool) => Test
printOnionTests = TestList [testPrintOnion, testPrintTernaryOnion]

testPrintOnion :: (?debug :: Bool) => Test
testPrintOnion = TestCase $ assertEqual
  "Test if onion prints correctly"
  "1 & 2"
  (display (Onion (PrimInt 1) (PrimInt 2)))

testPrintTernaryOnion :: (?debug :: Bool) => Test
testPrintTernaryOnion = TestCase $ assertEqual
  "Test if ternary onion prints correctly"
  "1 & 2 & 3"
  (display (Onion (PrimInt 1) (Onion (PrimInt 2) (PrimInt 3))))


-- Test cases that check pretty printing of basic builtin operators
-- printOpsTests :: (?debug :: Bool) => Test
-- printOpsTests = TestList [testPrintPlus, testPrintMinus, testPrintEqual]

-- testPrintPlus :: (?debug :: Bool) => Test
-- testPrintPlus = TestCase $ assertEqual
--   "Test that primitive add prints correctly"
--   "1 [+] 1"
--   (display (Plus (PrimInt 1) (PrimInt 1)))

-- testPrintMinus :: (?debug :: Bool) => Test
-- testPrintMinus = TestCase $ assertEqual
--   "Test that primitive subtract prints correctly"
--   "1 [-] 1"
--   (display (Minus (PrimInt 1) (PrimInt 1)))

-- testPrintEqual :: (?debug :: Bool) => Test
-- testPrintEqual = TestCase $ assertEqual
--   "Test that primitive equals prints correctly"
--   "1 [=] 1"
--   (display (Equal (PrimInt 1) (PrimInt 1)))