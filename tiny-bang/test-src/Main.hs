{-|
  The primary test module for TinyBang.
-}

module Main
( main
) where

import Control.Applicative ((<$>))
import Test.Framework
import Test.Framework.Providers.HUnit

import Test.TinyBang.SourceFile

testsM :: IO [Test]
testsM = sequence
  [ testGroup "source file tests" <$> hUnitTestToTests <$> sourceFileTests
  ]  

main::IO()
main = do
  tests <- testsM
  defaultMain tests
