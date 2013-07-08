{-|
  The primary test module for PatBang.
-}

module Main
( main
) where

import Control.Applicative ((<$>))
import Test.Framework
import Test.Framework.Providers.HUnit

import Test.Data.NFA
import Test.PatBang.Language.TypeSystem.Contours
import Test.PatBang.SourceFile

testsM :: IO [Test]
testsM = sequence
  [ testGroup "source file tests" <$> hUnitTestToTests <$> sourceFileTests
  , return $ testGroup "NFA tests" $ hUnitTestToTests nfaTests
  , return $ testGroup "Contour tests" $ hUnitTestToTests contourTests
  ]  

main::IO()
main = do
  tests <- testsM
  defaultMain tests
