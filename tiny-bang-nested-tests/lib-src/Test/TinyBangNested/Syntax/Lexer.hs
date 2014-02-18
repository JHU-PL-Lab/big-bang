{-|
  This module tests the TinyBangNested Lexer.
-}
module Test.TinyBangNested.Syntax.Lexer
( lexerTests
) where

import Language.TinyBang.Syntax.Location
import Language.TinyBangNested.Syntax.Lexer
import Test.HUnit

-- | Utility functions for Lexer unit tests:

-- | Converts [PositionalToken] to [Token] to allow comparison then 
--   tests for equality.
compareTokenStreams :: [Token] -> [PositionalToken] -> Bool
compareTokenStreams tokens posTokens = 
 tokens == map posToken posTokens

createLexerTest :: String -> String -> [Token] -> Test
createLexerTest name input expected =
  TestLabel name $ TestCase $ assertBool "token stream mismatch" boolResult
    where boolResult = compareTokenStreams expected lexerResult
          lexerResult = 
            case lexTinyBangNested UnknownDocument input of
              Left s -> error $ "Lexer unit test fail: " ++ s
              Right x -> x                     

lexerTests :: Test
lexerTests = TestList [testEmpty,testReservedWords, testOperators, testIdentifiers, testLiterals]

-- | Empty String
testEmpty :: Test
testEmpty = createLexerTest "Lexing empty string" "" [] 

-- | Reserved Words
testReservedWords :: Test
testReservedWords = createLexerTest  "Lexing reserved words" "int fun let in" [TokInt, TokFun, TokLet, TokIn]

-- | Identifiers
testIdentifiers :: Test
testIdentifiers = createLexerTest "Lexing identifiers" "xyz `xyz" [TokIdentifier "xyz", TokLabel "xyz"]

-- | Literals
testLiterals :: Test
testLiterals = createLexerTest "Lexing literals" "123 'a'" [TokLitInt 123, TokLitChar 'a']

-- | Operators
testOperators :: Test
testOperators = createLexerTest "Lexing operators" "= -> & () ( ) + - == >= <=" testOperatorsExpected
testOperatorsExpected :: [Token]
testOperatorsExpected = [TokIs, TokArrow, TokOnion, TokEmptyOnion, TokOpenParen, TokCloseParen, TokPlus, TokMinus, TokEq, TokGreaterEq, TokLessEq]
