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
import Language.TinyBang.Toploop
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


-- | Wrapper for evaluation
getEvaluatedResult :: String -> String
getEvaluatedResult input = stringyInterpretSource testConfig input

testConfig :: InterpreterConfiguration
testConfig = InterpreterConfiguration True True Simple



extractRight :: Either String b -> b
extractRight (Left l) = error l
extractRight (Right r) = r

genUnitTest :: String -> String -> String -> Test
genUnitTest label input expected = TestCase $ assertBool label (runTest input expected)

-- | runTest takes an input string, lexes and parses it to an Expr, runs the translator on it, 
-- | evaluates the translated expression then compares the output to an expected value.
-- | match.
runTest :: String -> String -> Bool
runTest input expected = if (verbose && not boolAnswer) 
                          then trace (result ++ "\nGave\n" ++ eval ++ "\nInstead of\n" ++ expected) $ boolAnswer 
                          else boolAnswer 
                           where
                           result = (render $ makeDoc $ performTransformation $ getParserResult $ getLexerResult input)
                           eval = getEvaluatedResult result
                           boolAnswer = (filterWhiteSpace eval) == (filterWhiteSpace expected)  

-- | Make unit tests a little more robust by ignoring white space
filterWhiteSpace :: String -> String
filterWhiteSpace s = filter keepChar s
                   where keepChar :: Char -> Bool
                         keepChar ' ' = False
                         keepChar '\n' = False
                         keepChar _ = True

testContext :: ParserContext
testContext = ParserContext UnknownDocument "ATranslationUnitTests"

aTranslationTests :: Test
aTranslationTests = TestList 
  [ testArithmetic
  , testNestedLabel
  , testDef
  , testVarIn
  , testOnion 
  , testScape
  , testPattern1
  , testPattern2
  , testProjector
  ]


testArithmetic :: Test
testArithmetic =  genUnitTest "Translating arithmetic" "8 + 5 -3 + 20" "30"

testNestedLabel :: Test
testNestedLabel =  genUnitTest "Translating nested label" "`A `B (6 + 5)" "`A `B 11"

testDef :: Test
testDef = genUnitTest "Translating def expression" "def x = 5 in def y = x + 5 in `Answer (x + y)" "`Answer 15" 

testVarIn :: Test
testVarIn = genUnitTest "Translating var in expression" "def x = 3 in x = x + 4 in x + 2" "9"

testOnion :: Test
testOnion = genUnitTest "Translating onion expression" "`A 1 & `B 2" "`A 1 & `B 2"

testScape :: Test
testScape = genUnitTest "Translating scape" "(p1:int -> p1 + 3) 8" "11"

testProjector :: Test
testProjector = genUnitTest "Translating projector" "(`A 1 & `B 2 & `C 3) &- `B" "`A 1 & `C 3"

testPattern1 :: Test
testPattern1 = genUnitTest "Translating patterns1" "(v1: `A v2:(`B v3:int & `C v4:int) -> v3 + v4) (`A `B 23 & `C 19)" "42"

testPattern2 :: Test
testPattern2 = genUnitTest "Translating patterns2" "(v: `A x:int & (`B y:int & (`C z:int & `D w:int)) -> x + y + z + w) (`A 1 & (`B 2 & (`C 3 & `D 4)))" "10"

