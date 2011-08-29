module Language.LittleBang.Syntax.ParserTest
( tests
) where

import qualified Language.LittleBang.Types.Types as T
import Language.LittleBang.Ast
import Language.LittleBang.Types.UtilTypes
import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Test.HUnit hiding (Label)
import Control.Exception

tests :: Test
tests = TestList [literalsCases, functionsCases, simpleCases, errorCases]

-- Test cases that check parsing of primitive literals works correctly
literalsCases :: Test
literalsCases = TestList [testParseInt, testParseChar, testParseUnit, testParseBool]

testParseInt :: Test
testParseInt = TestCase $ assertEqual
  "Input of 1234567890 should return PrimInt 1234567890"
  (PrimInt 1234567890)
  (parseLittleBang $ lexLittleBang "1234567890")

testParseChar :: Test
testParseChar = TestCase $ assertEqual
  "Input of \'a\' should return PrimChar \'a\'"
  (PrimChar 'a')
  (parseLittleBang $ lexLittleBang "\'a\'")

testParseUnit :: Test
testParseUnit = TestCase $ assertEqual
  "Input of () should return PrimUnit"
  PrimUnit
  (parseLittleBang $ lexLittleBang "()")

testParseBool :: Test
testParseBool = TestCase $ do
  assertEqual
    "Test parsing of `True ()"
    (Label (labelName "True") PrimUnit)
    (parseLittleBang $ lexLittleBang "`True ()")
  assertEqual
    "Test parsing of `False ()"
    (Label (labelName "False") PrimUnit)
    (parseLittleBang $ lexLittleBang "`False ()")


-- Test cases that check correctness in parsing function definitions and application
functionsCases :: Test
functionsCases = TestList [testLambdaExpr, testFuncAppl, testPerverseFunction, testFuncIgnoreNewLines, testCaseFunc]

testLambdaExpr :: Test
testLambdaExpr = TestCase $ assertEqual
  "Identity function: (\\x -> x)"
  (Func (ident "x") (Var (ident "x")))
  (parseLittleBang $ lexLittleBang "(\\x -> x)")

testFuncAppl :: Test
testFuncAppl = TestCase $ assertEqual
  "Test parsing of a function application"
  (Appl (Appl (Var (ident "plus")) (PrimInt 2)) (PrimInt 2))
  (parseLittleBang $ lexLittleBang "plus 2 2")

testPerverseFunction :: Test
testPerverseFunction = TestCase $ assertEqual
  "(fun x -> x x) (fun x -> x x)"
  (Appl (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x")))) (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x"))))) 
  (parseLittleBang $ lexLittleBang "(fun x -> x x) (fun x -> x x)")

testFuncIgnoreNewLines :: Test
testFuncIgnoreNewLines = TestCase $ assertEqual
  "Test if parser ignores newlines correctly"
  (Appl (Func (ident "x") (Var (ident "x"))) (Func (ident "x") (Var (ident "x"))))
  (parseLittleBang $ lexLittleBang "(\\x->x)\
                             \(\\x->x)")

-- TODO: Also test binders
testCaseFunc :: Test
testCaseFunc = TestCase $ assertEqual
               "Test if case block in function is parsed correctly"
               (Func (ident "x") (Case (Var (ident "x")) [(Nothing, ChiLabel (labelName "True") (ident "a"), PrimInt 1), (Nothing, ChiLabel (labelName "False") (ident "a"), PrimInt 0)]))
               (parseLittleBang $ lexLittleBang "(fun x -> case x of {\
                                                    \    `True a -> 1;\
                                                    \    `False a -> 0})")
                                          


-- Test cases for simple programs that should parse correctly (but may not have any interpreted meaning or use)
simpleCases :: Test
simpleCases = TestList [testFakeString, testCaseOfBlock, testTernaryOnion]

testFakeString :: Test
testFakeString = TestCase $ assertEqual
  "Testing \"fake\" strings"
  (Appl (Appl (Appl (Appl (Appl (PrimChar 's') (PrimChar 't')) (PrimChar 'r')) (PrimChar 'i')) (PrimChar 'n')) (PrimChar 'g')) 
  (parseLittleBang $ lexLittleBang "'s''t''r''i''n''g'")

testCaseOfBlock :: Test
testCaseOfBlock = TestCase $ assertEqual
  "Testing case...of block"
  (Case (Var (ident "x")) [(Nothing, ChiPrim T.PrimInt, PrimInt 5), (Nothing, ChiPrim T.PrimChar, PrimChar 'a'), (Nothing, ChiPrim T.PrimUnit, PrimUnit), (Nothing, ChiLabel (labelName "True") (ident "a"), Label (labelName "False") PrimUnit), (Nothing, ChiFun, Func (ident "x") (Var (ident "x")))])
  (parseLittleBang $ lexLittleBang "case x of {\
                             \    int -> 5;\
                             \    char -> \'a\';\
                             \    unit -> ();\
                             \    `True a -> `False ();\
                             \    fun -> (\\x -> x)}")

testTernaryOnion :: Test
testTernaryOnion = TestCase $ assertEqual
  "Testing ternary onion"
  (Onion (PrimInt 1) (Onion (PrimChar 'x') (Func (ident "x") (Var (ident "x")))))
  (parseLittleBang $ lexLittleBang "(1 & ('x' & (\\x -> x)))")


-- Tests cases that should throw a parser error
errorCases :: Test
errorCases = TestList [testParseEmptyString, testUnbalancedParens, testSemicolonEOL, testSemicolonCaseBlock, testEmptyCaseBlock]
 
testParseEmptyString :: Test
testParseEmptyString = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (parseLittleBang  $! lexLittleBang "")
      assertFailure "Input of \"\" should throw a parse error"

testUnbalancedParens :: Test
testUnbalancedParens = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (parseLittleBang $! lexLittleBang "(expr")
      assertFailure "Unbalanced parens should throw a parse error"

testSemicolonEOL :: Test
testSemicolonEOL = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (parseLittleBang $! lexLittleBang "square x;")
      assertFailure "Semicolon at end of line should throw a parse error"

testSemicolonCaseBlock :: Test
testSemicolonCaseBlock = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (parseLittleBang $! lexLittleBang "case x of {\nint -> 3;}")
      assertFailure "Semicolon before close brace in case...of block should throw a parse error"

testEmptyCaseBlock :: Test
testEmptyCaseBlock = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (parseLittleBang $! lexLittleBang "case x of {}")
      assertFailure "Empty case...of block should throw a parse error"
