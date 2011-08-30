module Language.LittleBang.Syntax.ParserTest
( tests
) where

import Test.HUnit hiding (Label)
import Control.Exception

import qualified Language.LittleBang.Types.Types as T
import Language.LittleBang.Ast
import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Language.LittleBang.Types.UtilTypes

tests :: Test
tests = TestList [literalsCases, functionsCases, simpleCases, errorCases]

-- TODO: replace once expectation-based testing is established
doParse :: String -> Expr
doParse src = case lexLittleBang src of
    Left err -> error $ "lexer error: " ++ err
    Right tokens -> parseLittleBang tokens

doStrictParse :: String -> Expr
doStrictParse = doParse
-- Note: the above does not work at the moment due to strictness problems.
-- However, it will be neatly corrected by expectation-based testing.

-- Test cases that check parsing of primitive literals works correctly
literalsCases :: Test
literalsCases = TestList [testParseInt, testParseChar, testParseUnit, testParseBool]

testParseInt :: Test
testParseInt = TestCase $ assertEqual
  "Input of 1234567890 should return PrimInt 1234567890"
  (PrimInt 1234567890)
  (doParse "1234567890")

testParseChar :: Test
testParseChar = TestCase $ assertEqual
  "Input of \'a\' should return PrimChar \'a\'"
  (PrimChar 'a')
  (doParse "\'a\'")

testParseUnit :: Test
testParseUnit = TestCase $ assertEqual
  "Input of () should return PrimUnit"
  PrimUnit
  (doParse "()")

testParseBool :: Test
testParseBool = TestCase $ do
  assertEqual
    "Test parsing of `True ()"
    (Label (labelName "True") PrimUnit)
    (doParse "`True ()")
  assertEqual
    "Test parsing of `False ()"
    (Label (labelName "False") PrimUnit)
    (doParse "`False ()")


-- Test cases that check correctness in parsing function definitions and application
functionsCases :: Test
functionsCases = TestList [testLambdaExpr, testFuncAppl, testPerverseFunction, testFuncIgnoreNewLines, testCaseFunc]

testLambdaExpr :: Test
testLambdaExpr = TestCase $ assertEqual
  "Identity function: (\\x -> x)"
  (Func (ident "x") (Var (ident "x")))
  (doParse "(\\x -> x)")

testFuncAppl :: Test
testFuncAppl = TestCase $ assertEqual
  "Test parsing of a function application"
  (Appl (Appl (Var (ident "plus")) (PrimInt 2)) (PrimInt 2))
  (doParse "plus 2 2")

testPerverseFunction :: Test
testPerverseFunction = TestCase $ assertEqual
  "(fun x -> x x) (fun x -> x x)"
  (Appl (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x")))) (Func (ident "x") (Appl (Var (ident "x")) (Var (ident "x"))))) 
  (doParse "(fun x -> x x) (fun x -> x x)")

testFuncIgnoreNewLines :: Test
testFuncIgnoreNewLines = TestCase $ assertEqual
  "Test if parser ignores newlines correctly"
  (Appl (Func (ident "x") (Var (ident "x"))) (Func (ident "x") (Var (ident "x"))))
  (doParse "(\\x->x)\
           \(\\x->x)")

-- TODO: Also test binders
testCaseFunc :: Test
testCaseFunc = TestCase $ assertEqual
               "Test if case block in function is parsed correctly"
               (Func (ident "x")
                 (Case (Var (ident "x"))
                    [ Branch Nothing (ChiLabel (labelName "True") (ident "a")) $ PrimInt 1
                    , Branch Nothing (ChiLabel (labelName "False") (ident "a")) $ PrimInt 0
                    ]
                 )
               )
               (doParse "(fun x -> case x of {\
                                              \    `True a -> 1;\
                                              \    `False a -> 0})")
                                          


-- Test cases for simple programs that should parse correctly (but may not have any interpreted meaning or use)
simpleCases :: Test
simpleCases = TestList [testFakeString, testCaseOfBlock, testTernaryOnion]

testFakeString :: Test
testFakeString = TestCase $ assertEqual
  "Testing \"fake\" strings"
  (Appl (Appl (Appl (Appl (Appl (PrimChar 's') (PrimChar 't')) (PrimChar 'r')) (PrimChar 'i')) (PrimChar 'n')) (PrimChar 'g')) 
  (doParse "'s''t''r''i''n''g'")

testCaseOfBlock :: Test
testCaseOfBlock = TestCase $ assertEqual
                  "Testing case...of block"
                  (Case (Var (ident "x"))
                    [ Branch Nothing (ChiPrim T.PrimInt) $ PrimInt 5
                    , Branch Nothing (ChiPrim T.PrimChar) $ PrimChar 'a'
                    , Branch Nothing (ChiPrim T.PrimUnit) PrimUnit
                    , Branch Nothing (ChiLabel (labelName "True") (ident "a")) $
                        Label (labelName "False") PrimUnit
                    , Branch Nothing ChiFun $ Func (ident "x") (Var (ident "x"))
                    ]
                  )
  (doParse "case x of {\
                       \    int -> 5;\
                       \    char -> \'a\';\
                       \    unit -> ();\
                       \    `True a -> `False ();\
                       \    fun -> (\\x -> x)}")

testTernaryOnion :: Test
testTernaryOnion = TestCase $ assertEqual
  "Testing ternary onion"
  (Onion (PrimInt 1) (Onion (PrimChar 'x') (Func (ident "x") (Var (ident "x")))))
  (doParse "(1 & ('x' & (\\x -> x)))")


-- Tests cases that should throw a parser error
errorCases :: Test
errorCases = TestList [testParseEmptyString, testUnbalancedParens, testSemicolonEOL, testSemicolonCaseBlock, testEmptyCaseBlock]
 
testParseEmptyString :: Test
testParseEmptyString = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (doStrictParse "")
      assertFailure "Input of \"\" should throw a parse error"

testUnbalancedParens :: Test
testUnbalancedParens = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (doStrictParse "(expr")
      assertFailure "Unbalanced parens should throw a parse error"

testSemicolonEOL :: Test
testSemicolonEOL = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (doStrictParse "square x;")
      assertFailure "Semicolon at end of line should throw a parse error"

testSemicolonCaseBlock :: Test
testSemicolonCaseBlock = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (doStrictParse "case x of {\nint -> 3;}")
      assertFailure "Semicolon before close brace in case...of block should throw a parse error"

testEmptyCaseBlock :: Test
testEmptyCaseBlock = TestCase $ do
  handleJust (\(ErrorCall a) -> Just a) (\_ -> return ()) performCall where
    performCall = do
      _ <- evaluate (doStrictParse "case x of {}")
      assertFailure "Empty case...of block should throw a parse error"
