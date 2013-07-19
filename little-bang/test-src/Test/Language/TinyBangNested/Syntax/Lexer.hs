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
compareTokenStreams tokens posTokens =  tokens == map convertToToken posTokens
                                        where
                                          convertToToken p = posToken p

-- | Function for automating calls to lexTinyBangNested and unwrapping result
getLexerResult :: String -> [PositionalToken]
getLexerResult input =  extractRight $ lexTinyBangNested "" input

extractRight :: Either a b -> b
extractRight (Left _) = error "extractRight called on Left"
extractRight (Right x) = x


lexerTests :: Test
lexerTests = TestList [testEmpty,testReservedWords, testOperators, testIdentifiers, testLiterals]

-- | Empty String
testEmpty :: Test
testEmpty = TestCase $ assertBool  "Lexing empty string"  $ compareTokenStreams testEmptyExpected  $ getLexerResult ""
testEmptyExpected :: [Token]
testEmptyExpected = []

-- | Reserved Words
testReservedWords :: Test
testReservedWords = TestCase $ assertBool "Lexing reserved words" $ compareTokenStreams testReservedWordsExpected $ getLexerResult "char int fun def in"
testReservedWordsExpected :: [Token]
testReservedWordsExpected = [TokChar, TokInt, TokFun, TokDef, TokIn]

-- | Operators
testOperators :: Test
testOperators = TestCase $ assertBool  "Lexing operators"  $ compareTokenStreams testOperatorsExpected  $ getLexerResult "= -> & () ( ) &- &. &! + - == > >= < <= :"
testOperatorsExpected :: [Token]
testOperatorsExpected = [TokIs, TokArrow, TokOnion, TokEmptyOnion, TokOpenParen, TokCloseParen, TokOnionSub, TokOnionProj, TokOnionSym, TokPlus, TokMinus, TokEq, TokGT, TokGTE, TokLT, TokLTE, TokColon]

-- | Identifiers
testIdentifiers :: Test
testIdentifiers = TestCase $ assertBool  "Lexing identifiers"  $ compareTokenStreams testIdentifiersExpected  $ getLexerResult "xyz `xyz"
testIdentifiersExpected :: [Token]
testIdentifiersExpected = [TokIdentifier "xyz", TokLabel "xyz"]

-- | Literals
testLiterals :: Test
testLiterals = TestCase $ assertBool  "Lexing literals"  $ compareTokenStreams testLiteralsExpected  $ getLexerResult "123 'a'"
testLiteralsExpected :: [Token]
testLiteralsExpected = [TokLitInt 123, TokLitChar 'a']


