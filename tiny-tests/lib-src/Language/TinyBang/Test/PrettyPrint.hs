module Language.TinyBang.Test.PrettyPrint
( tests
) where

import Test.HUnit hiding (Label)
import Language.TinyBang.Ast
import Language.TinyBang.Types.UtilTypes hiding (PrimitiveType(..))
import Data.ExtensibleVariant
import Utils.Render.Display
import qualified Language.TinyBang.Config as Cfg

-- TODO: Refactor this or eliminate it.
-- TODO: Use quickcheck to write a test that pretty print + parse is idempotent.

tests :: (?conf :: Cfg.Config) => Test
tests = TestList [printPrimCases, printFuncTests, printOnionTests{-, printOpsTests-}]

-- Test cases that ensure that primitive literals are printed correctly
printPrimCases :: (?conf :: Cfg.Config) => Test
printPrimCases = TestList [testPrintPositiveInt, testPrintNegativeInt, testPrintChar, testPrintVar, testPrintUnit, testPrintBoolean]

displayAst :: (?conf :: Cfg.Config) => ExprPart Expr -> String
displayAst a = display $ ((inj $ a) :: Expr)

testPrintPositiveInt :: (?conf :: Cfg.Config) => Test
testPrintPositiveInt = TestCase $ assertEqual
  "Test if input 1234567890 is printed correctly"
  "1234567890"
  (displayAst (PrimInt 1234567890))

testPrintNegativeInt :: (?conf :: Cfg.Config) => Test
testPrintNegativeInt = TestCase $ assertEqual
  "Test if input -1234567890 is printed correctly"
  "-1234567890"
  (displayAst (PrimInt (-1234567890)))

testPrintChar :: (?conf :: Cfg.Config) => Test
testPrintChar = TestCase $ assertEqual
  "Test if input \'a\' prints correctly"
  "'a'"
  (displayAst (PrimChar 'a'))

testPrintVar :: (?conf :: Cfg.Config) => Test
testPrintVar = TestCase $ assertEqual
  "Test if variables are printed correctly"
  "x"
  (displayAst (Var (ident "x")))

testPrintUnit :: (?conf :: Cfg.Config) => Test
testPrintUnit = TestCase $ assertEqual
  "Test if unit prints correctly"
  "()"
  (displayAst PrimUnit)

testPrintBoolean :: (?conf :: Cfg.Config) => Test
testPrintBoolean = TestCase $ do
  assertEqual
    "Test is boolean True prints correctly"
    "`True ()"
    (displayAst (Label (labelName "True") Nothing $ inj $ PrimUnit))
  assertEqual
    "Test is boolean False prints correctly"
    "`False ()"
    (displayAst (Label (labelName "False") Nothing $ inj $ PrimUnit))

-- Test cases that check pretty printing of function definitions and applications
printFuncTests :: (?conf :: Cfg.Config) => Test
printFuncTests = TestList [testPrintFunction1, testPrintFunction2, testPrintFuncAppl1, testPrintFuncAppl2, testPrintPerverse, testPrintFunction3]

testPrintFunction1 :: (?conf :: Cfg.Config) => Test
testPrintFunction1 = TestCase $ assertEqual
  "Test if I combinator is printed correctly"
  "(fun x -> x)"
  (displayAst (Func (ident "x") (inj $ Var (ident "x"))))

testPrintFunction2 :: (?conf :: Cfg.Config) => Test
testPrintFunction2 = TestCase $ assertEqual
  "Test if K combinator is printed correctly"
  "(fun x -> (fun y -> x))"
  (displayAst (Func (ident "x") (inj $ Func (ident "y") (inj $ Var (ident "x")))))

testPrintFuncAppl1 :: (?conf :: Cfg.Config) => Test
testPrintFuncAppl1 = TestCase $ assertEqual
  "Test if function application is printed correctly"
  "((plus 2) 2)"
  (displayAst (Appl (inj $ Appl (inj $ Var (ident "plus")) (inj $ PrimInt 2)) (inj $ PrimInt 2)))

testPrintFuncAppl2 :: (?conf :: Cfg.Config) => Test
testPrintFuncAppl2 = TestCase $ assertEqual
  "Test if printing function with variables and characters displays correctly"
  "((plus x) \'x\')"
  (displayAst (Appl (inj $ Appl (inj $ Var (ident "plus")) (inj $ Var (ident "x"))) (inj $ PrimChar 'x')))


testPrintPerverse :: (?conf :: Cfg.Config) => Test
testPrintPerverse = TestCase $ assertEqual
  "Test if perverse function application is printed correctly"
  "((fun x -> (x x)) (fun x -> (x x)))"
  (displayAst (Appl (inj $ Func (ident "x") (inj $ Appl (inj $ Var (ident "x")) (inj $ Var (ident "x")))) (inj $ Func (ident "x") (inj $ Appl (inj $ Var (ident "x")) (inj $ Var (ident "x"))))))

testPrintFunction3 :: (?conf :: Cfg.Config) => Test
testPrintFunction3 = TestCase $ assertEqual
  "Test if S combinator is pretty printed correctly"
  "(fun x -> (fun y -> (fun z -> ((x z) (y z)))))"
  (displayAst (Func (ident "x") (inj $ Func (ident "y") (inj $ Func (ident "z") (inj $ Appl (inj $ Appl (inj $ Var (ident "x")) (inj $ Var (ident "z"))) (inj $ Appl (inj $ Var (ident "y")) (inj $ Var (ident "z"))))))))


-- Test cases that check pretty printing of onions
printOnionTests :: (?conf :: Cfg.Config) => Test
printOnionTests = TestList [testPrintOnion, testPrintTernaryOnion]

testPrintOnion :: (?conf :: Cfg.Config) => Test
testPrintOnion = TestCase $ assertEqual
  "Test if onion prints correctly"
  "1 & 2"
  (displayAst (Onion (inj $ PrimInt 1) (inj $ PrimInt 2)))

testPrintTernaryOnion :: (?conf :: Cfg.Config) => Test
testPrintTernaryOnion = TestCase $ assertEqual
  "Test if ternary onion prints correctly"
  "1 & 2 & 3"
  (displayAst (Onion (inj $ PrimInt 1) (inj $ Onion (inj $ PrimInt 2) (inj $ PrimInt 3))))


-- Test cases that check pretty printing of basic builtin operators
-- printOpsTests :: (?conf :: Cfg.Config) => Test
-- printOpsTests = TestList [testPrintPlus, testPrintMinus, testPrintEqual]

-- testPrintPlus :: (?conf :: Cfg.Config) => Test
-- testPrintPlus = TestCase $ assertEqual
--   "Test that primitive add prints correctly"
--   "1 [+] 1"
--   (display (Plus (PrimInt 1) (PrimInt 1)))

-- testPrintMinus :: (?conf :: Cfg.Config) => Test
-- testPrintMinus = TestCase $ assertEqual
--   "Test that primitive subtract prints correctly"
--   "1 [-] 1"
--   (display (Minus (PrimInt 1) (PrimInt 1)))

-- testPrintEqual :: (?conf :: Cfg.Config) => Test
-- testPrintEqual = TestCase $ assertEqual
--   "Test that primitive equals prints correctly"
--   "1 [=] 1"
--   (display (Equal (PrimInt 1) (PrimInt 1)))