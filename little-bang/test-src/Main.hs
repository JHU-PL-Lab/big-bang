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

testsM :: IO [Test]
testsM = sequence
  [return $ testGroup "Lexer tests" $ hUnitTestToTests $ lexerTests, return $ testGroup "Parser tests" $ hUnitTestToTests $ parserTests]  

main :: IO()
main = do
  tests <- testsM
  defaultMain tests
