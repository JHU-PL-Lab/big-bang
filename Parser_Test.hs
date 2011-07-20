module Main where

import Language.BigBang.Ast
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Syntax.Lexer
import Language.BigBang.Syntax.Parser
import Test.HUnit hiding (Label)
import Control.Exception

edgeCases = TestList [testParseEmptyString]
 
testParseEmptyString = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      evaluate (parseBigBang $ lexBigBang "")
      assertFailure "Input of \"\" should throw a parse error"

simpleCases = TestList [testParseInt, testLambdaExpr, testPerverseFunction, testFakeString, testTernaryOnion, testFakeBool]
testParseInt = TestCase $ assertEqual
  "Input of 1234567890 should return PrimInt 1234567890"
  (PrimInt 1234567890)
  (parseBigBang $ lexBigBang "1234567890")

testLambdaExpr = TestCase $ assertEqual
  "Identity function: (\\x -> x)"
  (Func (ident "x") (Var (ident "x")))
  (parseBigBang $ lexBigBang "(\\x -> x)")

testPerverseFunction = TestCase $ assertEqual
  "(fun x -> x x) (fun x -> x x)"
  (Appl (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x")))) (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x"))))) 
  (parseBigBang $ lexBigBang "(fun x -> x x) (fun x -> x x)")

testFakeString = TestCase $ assertEqual
  "Testing \"fake\" strings"
  (Appl (Appl (Appl (Appl (Appl (PrimChar 's') (PrimChar 't')) (PrimChar 'r')) (PrimChar 'i')) (PrimChar 'n')) (PrimChar 'g')) 
  (parseBigBang $ lexBigBang "'s''t''r''i''n''g'")

testTernaryOnion = TestCase $ assertEqual
  "Testing ternary onion"
  (Onion (PrimInt 1) (Onion (PrimChar 'x') (Func (ident "x") (Var (ident "x")))))
  (parseBigBang $ lexBigBang "(1 & ('x' & (\\x -> x)))")

testFakeBool = TestCase $ assertEqual
  "Testing \"fake\" boolean"
  (Label (labelName "true") PrimUnit)
  (parseBigBang $ lexBigBang "`true ()")

testCases = TestList [edgeCases, simpleCases]
main = runTestTT testCases
