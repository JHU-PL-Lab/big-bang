module Language.LittleBang.Test.PrettyPrint
( tests
) where

import Test.HUnit hiding (Label)
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import Language.TinyBang.Types.UtilTypes hiding (PrimitiveType(..))
import Utils.Language.Ast
import Utils.Render.Display
import qualified Language.TinyBang.Config as Cfg

-- TODO: Refactor this or eliminate it.
-- TODO: Use quickcheck to write a test that pretty print + parse is idempotent.

tests :: (?conf :: Cfg.Config) => Test
tests = TestList [printPrimCases, printFuncTests, printOnionTests{-, printOpsTests-}]

-- Test cases that ensure that primitive literals are printed correctly
printPrimCases :: (?conf :: Cfg.Config) => Test
printPrimCases = TestList [testPrintPositiveInt, testPrintNegativeInt, testPrintChar, testPrintVar, testPrintUnit, testPrintBoolean]

-- A display function used to force AST type
displayAst :: (?conf :: Cfg.Config) => LA.Expr -> String
displayAst = display

testPrintPositiveInt :: (?conf :: Cfg.Config) => Test
testPrintPositiveInt = TestCase $ assertEqual
  "Test if input 1234567890 is printed correctly"
  "1234567890"
  (displayAst (astwrap $ TA.PrimInt 1234567890))

testPrintNegativeInt :: (?conf :: Cfg.Config) => Test
testPrintNegativeInt = TestCase $ assertEqual
  "Test if input -1234567890 is printed correctly"
  "-1234567890"
  (displayAst (astwrap $ TA.PrimInt (-1234567890)))

testPrintChar :: (?conf :: Cfg.Config) => Test
testPrintChar = TestCase $ assertEqual
  "Test if input \'a\' prints correctly"
  "'a'"
  (displayAst (astwrap $ TA.PrimChar 'a'))

testPrintVar :: (?conf :: Cfg.Config) => Test
testPrintVar = TestCase $ assertEqual
  "Test if variables are printed correctly"
  "x"
  (displayAst (astwrap $ TA.Var (ident "x")))

testPrintUnit :: (?conf :: Cfg.Config) => Test
testPrintUnit = TestCase $ assertEqual
  "Test if unit prints correctly"
  "()"
  (displayAst $ astwrap $ TA.PrimUnit)

testPrintBoolean :: (?conf :: Cfg.Config) => Test
testPrintBoolean = TestCase $ do
  assertEqual
    "Test is boolean True prints correctly"
    "`True ()"
    (displayAst (astwrap $ TA.Label (labelName "True") Nothing $ astwrap $ TA.PrimUnit))
  assertEqual
    "Test is boolean False prints correctly"
    "`False ()"
    (displayAst (astwrap $ TA.Label (labelName "False") Nothing $ astwrap $ TA.PrimUnit))

-- Test cases that check pretty printing of TA.Function definitions and TA.Applications
printFuncTests :: (?conf :: Cfg.Config) => Test
printFuncTests = TestList [testPrintFunction1, testPrintFunction2, testPrintFuncAppl1, testPrintFuncAppl2, testPrintPerverse, testPrintFunction3]

testPrintFunction1 :: (?conf :: Cfg.Config) => Test
testPrintFunction1 = TestCase $ assertEqual
  "Test if I combinator is printed correctly"
  "(fun x -> x)"
  (displayAst (astwrap $ TA.Func (ident "x") (astwrap $ TA.Var (ident "x"))))

testPrintFunction2 :: (?conf :: Cfg.Config) => Test
testPrintFunction2 = TestCase $ assertEqual
  "Test if K combinator is printed correctly"
  "(fun x -> (fun y -> x))"
  (displayAst (astwrap $ TA.Func (ident "x") (astwrap $ TA.Func (ident "y") (astwrap $ TA.Var (ident "x")))))

testPrintFuncAppl1 :: (?conf :: Cfg.Config) => Test
testPrintFuncAppl1 = TestCase $ assertEqual
  "Test if TA.Function TA.Application is printed correctly"
  "((plus 2) 2)"
  (displayAst (astwrap $ TA.Appl (astwrap $ TA.Appl (astwrap $ TA.Var (ident "plus")) (astwrap $ TA.PrimInt 2)) (astwrap $ TA.PrimInt 2)))

testPrintFuncAppl2 :: (?conf :: Cfg.Config) => Test
testPrintFuncAppl2 = TestCase $ assertEqual
  "Test if printing TA.Function with variables and characters displays correctly"
  "((plus x) \'x\')"
  (displayAst (astwrap $ TA.Appl (astwrap $ TA.Appl (astwrap $ TA.Var (ident "plus")) (astwrap $ TA.Var (ident "x"))) (astwrap $ TA.PrimChar 'x')))


testPrintPerverse :: (?conf :: Cfg.Config) => Test
testPrintPerverse = TestCase $ assertEqual
  "Test if perverse TA.Function TA.Application is printed correctly"
  "((fun x -> (x x)) (fun x -> (x x)))"
  (displayAst (astwrap $ TA.Appl (astwrap $ TA.Func (ident "x") (astwrap $ TA.Appl (astwrap $ TA.Var (ident "x")) (astwrap $ TA.Var (ident "x")))) (astwrap $ TA.Func (ident "x") (astwrap $ TA.Appl (astwrap $ TA.Var (ident "x")) (astwrap $ TA.Var (ident "x"))))))

testPrintFunction3 :: (?conf :: Cfg.Config) => Test
testPrintFunction3 = TestCase $ assertEqual
  "Test if S combinator is pretty printed correctly"
  "(fun x -> (fun y -> (fun z -> ((x z) (y z)))))"
  (displayAst (astwrap $ TA.Func (ident "x") (astwrap $ TA.Func (ident "y") (astwrap $ TA.Func (ident "z") (astwrap $ TA.Appl (astwrap $ TA.Appl (astwrap $ TA.Var (ident "x")) (astwrap $ TA.Var (ident "z"))) (astwrap $ TA.Appl (astwrap $ TA.Var (ident "y")) (astwrap $ TA.Var (ident "z"))))))))


-- Test cases that check pretty printing of onions
printOnionTests :: (?conf :: Cfg.Config) => Test
printOnionTests = TestList [testPrintOnion, testPrintTernaryOnion]

testPrintOnion :: (?conf :: Cfg.Config) => Test
testPrintOnion = TestCase $ assertEqual
  "Test if onion prints correctly"
  "1 & 2"
  (displayAst (astwrap $ LA.Onion (astwrap $ TA.PrimInt 1) (astwrap $ TA.PrimInt 2)))

testPrintTernaryOnion :: (?conf :: Cfg.Config) => Test
testPrintTernaryOnion = TestCase $ assertEqual
  "Test if ternary onion prints correctly"
  "1 & 2 & 3"
  (displayAst (astwrap $ LA.Onion (astwrap $ TA.PrimInt 1) (astwrap $ LA.Onion (astwrap $ TA.PrimInt 2) (astwrap $ TA.PrimInt 3))))


-- Test cases that check pretty printing of basic builtin operators
-- printOpsTests :: (?conf :: Cfg.Config) => Test
-- printOpsTests = TestList [testPrintPlus, testPrintMinus, testPrintEqual]

-- testPrintPlus :: (?conf :: Cfg.Config) => Test
-- testPrintPlus = TestCase $ assertEqual
--   "Test that primitive add prints correctly"
--   "1 [+] 1"
--   (display (Plus (TA.PrimInt 1) (TA.PrimInt 1)))

-- testPrintMinus :: (?conf :: Cfg.Config) => Test
-- testPrintMinus = TestCase $ assertEqual
--   "Test that primitive subtract prints correctly"
--   "1 [-] 1"
--   (display (Minus (TA.PrimInt 1) (TA.PrimInt 1)))

-- testPrintEqual :: (?conf :: Cfg.Config) => Test
-- testPrintEqual = TestCase $ assertEqual
--   "Test that primitive equals prints correctly"
--   "1 [=] 1"
--   (display (Equal (TA.PrimInt 1) (TA.PrimInt 1)))
