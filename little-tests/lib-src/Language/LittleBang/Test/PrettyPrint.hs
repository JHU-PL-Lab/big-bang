module Language.LittleBang.Test.PrettyPrint
( tests
) where

import Test.HUnit hiding (Label)
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
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

-- A display function used to force AST type
displayAst :: (?conf :: Cfg.Config) => LA.Expr -> String
displayAst = display

testPrintPositiveInt :: (?conf :: Cfg.Config) => Test
testPrintPositiveInt = TestCase $ assertEqual
  "Test if input 1234567890 is printed correctly"
  "1234567890"
  (displayAst (inj $ TA.PrimInt 1234567890))

testPrintNegativeInt :: (?conf :: Cfg.Config) => Test
testPrintNegativeInt = TestCase $ assertEqual
  "Test if input -1234567890 is printed correctly"
  "-1234567890"
  (displayAst (inj $ TA.PrimInt (-1234567890)))

testPrintChar :: (?conf :: Cfg.Config) => Test
testPrintChar = TestCase $ assertEqual
  "Test if input \'a\' prints correctly"
  "'a'"
  (displayAst (inj $ TA.PrimChar 'a'))

testPrintVar :: (?conf :: Cfg.Config) => Test
testPrintVar = TestCase $ assertEqual
  "Test if variables are printed correctly"
  "x"
  (displayAst (inj $ TA.Var (ident "x")))

testPrintUnit :: (?conf :: Cfg.Config) => Test
testPrintUnit = TestCase $ assertEqual
  "Test if unit prints correctly"
  "()"
  (displayAst $ inj $ TA.PrimUnit)

testPrintBoolean :: (?conf :: Cfg.Config) => Test
testPrintBoolean = TestCase $ do
  assertEqual
    "Test is boolean True prints correctly"
    "`True ()"
    (displayAst (inj $ TA.Label (labelName "True") Nothing $ inj $ TA.PrimUnit))
  assertEqual
    "Test is boolean False prints correctly"
    "`False ()"
    (displayAst (inj $ TA.Label (labelName "False") Nothing $ inj $ TA.PrimUnit))

-- Test cases that check pretty printing of TA.Function definitions and TA.Applications
printFuncTests :: (?conf :: Cfg.Config) => Test
printFuncTests = TestList [testPrintFunction1, testPrintFunction2, testPrintFuncAppl1, testPrintFuncAppl2, testPrintPerverse, testPrintFunction3]

testPrintFunction1 :: (?conf :: Cfg.Config) => Test
testPrintFunction1 = TestCase $ assertEqual
  "Test if I combinator is printed correctly"
  "(fun x -> x)"
  (displayAst (inj $ TA.Func (ident "x") (inj $ TA.Var (ident "x"))))

testPrintFunction2 :: (?conf :: Cfg.Config) => Test
testPrintFunction2 = TestCase $ assertEqual
  "Test if K combinator is printed correctly"
  "(fun x -> (fun y -> x))"
  (displayAst (inj $ TA.Func (ident "x") (inj $ TA.Func (ident "y") (inj $ TA.Var (ident "x")))))

testPrintFuncAppl1 :: (?conf :: Cfg.Config) => Test
testPrintFuncAppl1 = TestCase $ assertEqual
  "Test if function application is printed correctly"
  "((plus 2) 2)"
  (displayAst (inj $ LA.Appl (inj $ LA.Appl (inj $ TA.Var (ident "plus")) (inj $ TA.PrimInt 2)) (inj $ TA.PrimInt 2)))

testPrintFuncAppl2 :: (?conf :: Cfg.Config) => Test
testPrintFuncAppl2 = TestCase $ assertEqual
  "Test if printing function with variables and characters displays correctly"
  "((plus x) \'x\')"
  (displayAst (inj $ LA.Appl (inj $ LA.Appl (inj $ TA.Var (ident "plus")) (inj $ TA.Var (ident "x"))) (inj $ TA.PrimChar 'x')))


testPrintPerverse :: (?conf :: Cfg.Config) => Test
testPrintPerverse = TestCase $ assertEqual
  "Test if perverse function application is printed correctly"
  "((fun x -> (x x)) (fun x -> (x x)))"
  (displayAst (inj $ LA.Appl (inj $ TA.Func (ident "x") (inj $ LA.Appl (inj $ TA.Var (ident "x")) (inj $ TA.Var (ident "x")))) (inj $ TA.Func (ident "x") (inj $ LA.Appl (inj $ TA.Var (ident "x")) (inj $ TA.Var (ident "x"))))))

testPrintFunction3 :: (?conf :: Cfg.Config) => Test
testPrintFunction3 = TestCase $ assertEqual
  "Test if S combinator is pretty printed correctly"
  "(fun x -> (fun y -> (fun z -> ((x z) (y z)))))"
  (displayAst (inj $ TA.Func (ident "x") (inj $ TA.Func (ident "y") (inj $ TA.Func (ident "z") (inj $ LA.Appl (inj $ LA.Appl (inj $ TA.Var (ident "x")) (inj $ TA.Var (ident "z"))) (inj $ LA.Appl (inj $ TA.Var (ident "y")) (inj $ TA.Var (ident "z"))))))))


-- Test cases that check pretty printing of onions
printOnionTests :: (?conf :: Cfg.Config) => Test
printOnionTests = TestList [testPrintOnion, testPrintTernaryOnion]

testPrintOnion :: (?conf :: Cfg.Config) => Test
testPrintOnion = TestCase $ assertEqual
  "Test if onion prints correctly"
  "1 & 2"
  (displayAst (inj $ LA.Onion (inj $ TA.PrimInt 1) (inj $ TA.PrimInt 2)))

testPrintTernaryOnion :: (?conf :: Cfg.Config) => Test
testPrintTernaryOnion = TestCase $ assertEqual
  "Test if ternary onion prints correctly"
  "1 & 2 & 3"
  (displayAst (inj $ LA.Onion (inj $ TA.PrimInt 1) (inj $ LA.Onion (inj $ TA.PrimInt 2) (inj $ TA.PrimInt 3))))


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