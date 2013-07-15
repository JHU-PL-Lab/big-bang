{-
  This module tests the TinyBangNested to TinyBangANormal translation process.
-}
module Test.ATranslation.Translator
( aTranslationTests
) where

import Debug.Trace
import qualified Language.TinyBangNested.Ast.Data as TBN
import qualified Language.TinyBang.Ast.Data as TBA
import Language.TinyBang.Display
import Language.TinyBang.Syntax.Location
import Language.TinyBangNested.Syntax.Parser
import Language.TinyBangNested.Syntax.Lexer
import ATranslation.Translator
import Test.HUnit

-- | Display unit tests?
verbose :: Bool
verbose = True

-- | Utility functions for ATranslation unit tests:

-- | Function for automating calls to lexTinyBangNested and unwrapping result
getLexerResult :: String -> [PositionalToken]
getLexerResult input =  extractRight $ lexTinyBangNested "" input

-- | Function for automating calls to parseTinyBangNested and displaying result
getParserResult :: [PositionalToken] -> TBN.Expr
getParserResult input =  extractRight $ parseTinyBangNested testContext input

extractRight :: Either String b -> b
extractRight (Left l) = error l
extractRight (Right r) = r

genUnitTest :: String -> String -> String -> Test
genUnitTest label input expected = TestCase $ assertBool label (runTest input expected)

-- | runTest takes an input string, lexes and parses it to an Expr, runs the translator on it, 
-- | then compares the translated output to an expected string. Returns true if the strings
-- | match.
runTest :: String -> String -> Bool
runTest input expected = if (verbose && not boolAnswer) then trace result $ boolAnswer else boolAnswer 
                         where
                           result = (render $ makeDoc $ performTransformation $ getParserResult $ getLexerResult input)
                           boolAnswer = (filterWhiteSpace result) == (filterWhiteSpace expected)  

filterWhiteSpace :: String -> String
filterWhiteSpace s = filter keepChar s
                   where keepChar :: Char -> Bool
                         keepChar ' ' = False
                         keepChar '\n' = False
                         keepChar _ = True

testContext :: ParserContext
testContext = ParserContext UnknownDocument "UnitTestDoc"

aTranslationTests :: Test
aTranslationTests = TestList [testArithmetic, testLabel, testNestedLabel, testDef, testVarIn, testOnion]

-- | Test simple addition
testArithmetic :: Test
testArithmetic =  genUnitTest "Translating arithmetic" "8 + 5 -3 + 20" "x0 = 8; x1 = 5;  x2 = x0 + x1; x3 = 3; x4 = x2 - x3;  x5 = 20; x6 = x4 + x5"

-- | Test label
testLabel :: Test
testLabel =  genUnitTest "Translating label" "`A 6" "x0 = 6; y0 := x0; x1 = `A y0"

-- | Test nested label + arithmetic
testNestedLabel :: Test
testNestedLabel =  genUnitTest "Translating nested label" "`A `B (6 + 5)" "x0 = 6; x1 = 5; x2 = x0 + x1; y0 := x2; x3 = `B y0; y1 := x3; x4 = `A y1"

-- | Test def expression
testDef :: Test
testDef = genUnitTest "Translating def expression" "def x = 5 in def y = x + 5 in `Answer (x + y)" "x0 = 5; y0 := x0; x1 = !y0; x2 = 5; x3 = x1 + x2; y1 := x3; x4 = !y0; x5 = !y1; x6 = x4 + x5; y2 := x6; x7 = `Answer y2" 

-- | Test def expression
testVarIn :: Test
testVarIn = genUnitTest "Translating var in expression" "def x = 3 in x = x + 4 in x + 2" "x0 = 3; y0 := x0; x1 = !y0; x2 = 4; x3 = x1 + x2; y0 <- x3; x4 = !y0; x5 = 2; x6 = x4 + x5"

-- | Test the onion operator (TODO: eval is wrong?)
testOnion :: Test
testOnion = genUnitTest "Translating onion expression" "`A 1 & `B 2 & 6" "x0 = 1; y0 := x0; x1 = `A y0; x2 = 2; y1 := x2; x3 = `B y1; x4 = x1 & x3; x5 = 6; x6 = x4 & x5"
