{-|
  The primary test module for TinyBangNested.
-}

module Main 
( main 
) where


import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Test.Language.TinyBangNested.Syntax.Lexer
import Test.Language.TinyBangNested.Syntax.Parser
import Test.ATranslation.Translator

tbnLexerTestGroup :: Test
tbnLexerTestGroup = testGroup "TinyBangNested lexer tests" $ hUnitTestToTests $ lexerTests
tbnParserTestGroup :: Test
tbnParserTestGroup = testGroup "TinyBangNested parser tests" $ hUnitTestToTests $ parserTests
aTranslationTestGroup :: Test
aTranslationTestGroup = testGroup "ATranslation tests" $ hUnitTestToTests $ aTranslationTests

testsM :: IO [Test]
testsM = sequence
  [return $ tbnLexerTestGroup, return $ tbnParserTestGroup, return $ aTranslationTestGroup]  

main :: IO()
main = do
  tests <- testsM
  defaultMain tests
