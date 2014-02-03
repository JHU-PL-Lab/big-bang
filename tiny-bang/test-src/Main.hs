{-|
  The primary test module for TinyBang.
-}

module Main
( main
) where

import Control.Applicative ((<$>))
import Data.Monoid
import Data.Maybe
import System.Environment
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit

import Language.TinyBang.Utils.Logger
import Options
import qualified Test.TinyBang.TypeSystem.NFA as NFA
import qualified Test.TinyBang.TypeSystem.Contours as Contours
import qualified Test.TinyBang.SourceFile as SourceFile

testsM :: IO [Test]
testsM = sequence
  [ return $ testGroup "NFA tests" $ hUnitTestToTests NFA.tests
  , return $ testGroup "Contour tests" $ hUnitTestToTests Contours.tests
  , testGroup "source file tests" <$> hUnitTestToTests <$> SourceFile.tests
  ]  

-- |The main for the TinyBang unit tests.  We accept more options than the
--  default test runner, so we have to bolt into the side of test-framework and
--  parse its options.  This is accomplished by building our own getOpt options
--  (as a record structure of @Maybe@ values) and tupling it with the option
--  structure from test-framework.
main :: IO ()
main = do
  args <- getArgs
  result <- parseOptions args
  case result of
    Left (msg,exitcode) -> do
      putStrLn msg
      exitWith exitcode
    Right (tfOpts,k3tOpts) -> do
      -- ## First, process K3 tester options
      -- Logger options first
      let loggerSettings = fromJust $ loggerInstructions k3tOpts
      mconcat <$> mapM configureByInstruction loggerSettings
      -- The type system override next
      let typeSystemFilter = fromJust $ sourceFileOnlyByName k3tOpts
      -- ## Then run the test-framework main
      tests' <- case typeSystemFilter of
                  Nothing -> testsM
                  Just filename ->
                    hUnitTestToTests <$> SourceFile.filteredTests filename
      defaultMainWithOpts tests' tfOpts
