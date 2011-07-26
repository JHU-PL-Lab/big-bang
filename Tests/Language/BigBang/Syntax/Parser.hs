module Tests.Language.BigBang.Syntax.Parser 
( testCases
) where

import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Ast
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Syntax.Lexer
import Language.BigBang.Syntax.Parser
import Test.HUnit hiding (Label)
import Control.Exception

edgeCases = TestList [testEmptyCaseBlock, testParseEmptyString, testUnbalancedParens, testSemicolonEOL, testSemicolonCaseBlock]
 
testParseEmptyString = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      evaluate (parseBigBang $ lexBigBang "")
      assertFailure "Input of \"\" should throw a parse error"

testUnbalancedParens = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      evaluate (parseBigBang $ lexBigBang "(expr")
      assertFailure "Unbalanced parens should throw a parse error"

testSemicolonEOL = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      evaluate (parseBigBang $ lexBigBang "square x;")
      assertFailure "Semicolon at end of line should throw a parse error"

testSemicolonCaseBlock = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      evaluate (parseBigBang $ lexBigBang "case x of {\nint -> 3;}")
      assertFailure "Semicolon before close brace in case...of block should throw a parse error"

testEmptyCaseBlock = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      evaluate (parseBigBang $ lexBigBang "case x of {}")
      assertFailure "Empty case...of block should throw a parse error"

simpleCases = TestList [testParseChar, testParseUnit, testFuncAppl, testParseInt, testLambdaExpr, testPerverseFunction, testFakeString, testTernaryOnion, testFakeBool, testIgnoreNewLines, testCaseOfBlock]
testParseInt = TestCase $ assertEqual
  "Input of 1234567890 should return PrimInt 1234567890"
  (PrimInt 1234567890)
  (parseBigBang $ lexBigBang "1234567890")

testParseChar = TestCase $ assertEqual
  "Input of \'a\' should return PrimChar \'a\'"
  (PrimChar 'a')
  (parseBigBang $ lexBigBang "\'a\'")

testParseUnit = TestCase $ assertEqual
  "Input of () should return PrimUnit"
  PrimUnit
  (parseBigBang $ lexBigBang "()")

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


testCaseOfBlock = TestCase $ assertEqual
  "Testing case...of block"
  (Case (Var (ident "x")) [(ChiPrim T.PrimInt, PrimInt 5), (ChiPrim T.PrimChar, PrimChar 'a'), (ChiPrim T.PrimUnit, PrimUnit), (ChiLabel (labelName "True") (ident "a"), Label (labelName "False") PrimUnit), (ChiFun, Func (ident "x") (Var (ident "x")))])
  (parseBigBang $ lexBigBang "case x of {\nint -> 5;\nchar -> \'a\';\nunit -> ();\n`True a -> `False ();fun -> (\\x -> x)}")

testIgnoreNewLines = TestCase $ assertEqual
  "Test if parser ignores newlines correctly"
  (Appl (Func (ident "x") (Var (ident "x"))) (Func (ident "x") (Var (ident "x"))))
  (parseBigBang $ lexBigBang "(\\x->x)\n(\\x->x)")

testFuncAppl = TestCase $ assertEqual
  "Test parsing of a function application"
  (Appl (Appl (Var (ident "plus")) (PrimInt 2)) (PrimInt 2))
  (parseBigBang $ lexBigBang "plus 2 2")

testCases = TestList [edgeCases, simpleCases]

