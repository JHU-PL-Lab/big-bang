{-
  This module tests the TinyBangNested Lexer.
-}
module Test.Language.TinyBangNested.Syntax.Lexer
( lexerTests
) where

import Language.TinyBangNested.Syntax.Lexer
import Test.HUnit

-- | Utility functions for Lexer unit tests:

-- | Converts [PositionalToken] to [Token] to allow comparison then 
--   tests for equality.
compareTokenStreams :: [Token] -> [PositionalToken] -> Bool
compareTokenStreams tokens posTokens = 
 tokens == map convertToToken posTokens
   where
    convertToToken p = posToken p

createLexerTest :: String -> String -> [Token] -> Test
createLexerTest name input expected =
  TestCase $ assertBool name $ boolResult
    where boolResult = compareTokenStreams expected lexerResult
          lexerResult = 
            case (lexTinyBangNested "" input) of
              Left s -> error $ "Lexer unit test fail: " ++ s
              Right x -> x                     

lexerTests :: Test
lexerTests = TestList [testEmpty,testReservedWords, testOperators, testIdentifiers, testLiterals]

-- | Empty String
testEmpty :: Test
testEmpty = createLexerTest "Lexing empty string" "" [] 

-- | Reserved Words
testReservedWords :: Test
testReservedWords = createLexerTest  "Lexing reserved words" "char int fun def in" [TokChar, TokInt, TokFun, TokDef, TokIn]

-- | Identifiers
testIdentifiers :: Test
testIdentifiers = createLexerTest "Lexing identifiers" "xyz `xyz" [TokIdentifier "xyz", TokLabel "xyz"]

-- | Literals
testLiterals :: Test
testLiterals = createLexerTest "Lexing literals" "123 'a'" [TokLitInt 123, TokLitChar 'a']

-- | Operators
testOperators :: Test
testOperators = createLexerTest "Lexing operators" "= -> & () ( ) &- &. &! + - == > >= < <= :" testOperatorsExpected
testOperatorsExpected :: [Token]
testOperatorsExpected = [TokIs, TokArrow, TokOnion, TokEmptyOnion, TokOpenParen, TokCloseParen, TokOnionSub, TokOnionProj, TokOnionSym, TokPlus, TokMinus, TokEq, TokGT, TokGTE, TokLT, TokLTE, TokColon]




