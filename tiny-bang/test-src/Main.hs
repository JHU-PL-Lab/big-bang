{-|
  The primary test module for TinyBang.
-}

module Main
( main
) where

import Control.Applicative ((<$>))
import Test.Framework
import Test.Framework.Providers.HUnit

import Test.Data.NFA
import Test.TinyBang.Language.TypeSystem.Contours
import Test.TinyBang.SourceFile

testsM :: IO [Test]
testsM = sequence
  [ return $ testGroup "NFA tests" $ hUnitTestToTests nfaTests
  , return $ testGroup "Contour tests" $ hUnitTestToTests contourTests
  , testGroup "source file tests" <$> hUnitTestToTests <$> sourceFileTests
  ]  

main::IO()
main = do
  tests <- testsM
  defaultMain tests
