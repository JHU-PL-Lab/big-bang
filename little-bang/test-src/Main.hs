{-|
  The primary test module for LittleBang.
-}

module Main 
( main 
) where


import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Test.Language.LittleBang.Syntax.Lexer
import Test.Language.LittleBang.Syntax.Parser
import Test.Translator.Translator

-- TODO: rework all of these tests for LittleBang

tbnLexerTestGroup :: Test
tbnLexerTestGroup = testGroup "LittleBang lexer tests" $ hUnitTestToTests $ lexerTests
tbnParserTestGroup :: Test
tbnParserTestGroup = testGroup "LittleBang parser tests" $ hUnitTestToTests $ parserTests
aTranslationTestGroup :: Test
aTranslationTestGroup = testGroup "Translator tests" $ hUnitTestToTests $ aTranslationTests

testsM :: IO [Test]
testsM = sequence
  [return $ tbnLexerTestGroup, return $ tbnParserTestGroup, return $ aTranslationTestGroup]  

main :: IO()
main = do
  tests <- testsM
  defaultMain tests
