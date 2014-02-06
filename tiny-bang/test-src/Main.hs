{-|
  The primary test module for TinyBang.
-}

module Main
( main
) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Monoid
import Data.Maybe
import System.Environment
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit

import Language.TinyBang.Utils.Assertions
import Language.TinyBang.Utils.Logger
import Options
import qualified Test.TinyBang.TypeSystem.NFA as NFA
import qualified Test.TinyBang.TypeSystem.Contours as Contours
import qualified Test.TinyBang.SourceFile as SourceFile

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
    Right (tfOpts,tbOpts) -> do
      -- ## First, process TinyBang tester options
      -- Logger options first
      let loggerSettings = fromJust $ loggerInstructions tbOpts
      mconcat <$> mapM configureByInstruction loggerSettings
      -- Assertion options next
      when (fromJust $ staticAssertions tbOpts) $ do
        enableAssertions
        print "Assertions enabled!"
      -- The type system override next
      let typeSystemFilter = fromJust $ sourceFileOnlyByName tbOpts
      -- Fetch the empty database
      let emptyDb = fromJust $ emptyDatabase tbOpts
      -- Build the source file options
      let sfOpts = SourceFile.SourceFileTestConfig
                    { SourceFile.sftFilter = typeSystemFilter
                    , SourceFile.sftDatabase = emptyDb 
                    }
      -- Construct the appropriate source file tests
      let sfTests = sequence
                      [ testGroup "source file tests" <$> hUnitTestToTests <$>
                          SourceFile.generateTests sfOpts
                      ]
      -- Construct the other tests
      let otherTests = return
            [ testGroup "NFA tests" $ hUnitTestToTests NFA.tests
            , testGroup "Contour tests" $ hUnitTestToTests Contours.tests
            ]
      -- Build the test list
      tests <- case typeSystemFilter of
                  Nothing -> concat <$> sequence [otherTests, sfTests]
                  Just _ -> sfTests
      defaultMainWithOpts tests tfOpts
