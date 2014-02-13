{-|
  The primary test module for TinyBang.
-}

module Main
( main
) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Monoid
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit

import Language.TinyBang.Utils.Assertions
import Language.TinyBang.Utils.Logger
import Test.TinyBang.Options
import qualified Test.TinyBang.TypeSystem.NFA as NFA
import qualified Test.TinyBang.TypeSystem.Contours as Contours
import qualified Test.TinyBang.SourceFile as SourceFile
import Test.Utils.TestFramework.Options

-- |The main for the TinyBang unit tests.  We accept more options than the
--  default test runner, so we have to bolt into the side of test-framework and
--  parse its options.  This is accomplished by building our own getOpt options
--  (as a record structure of @Maybe@ values) and tupling it with the option
--  structure from test-framework.
main :: IO ()
main =
  defaultMainWithCustomOpts tinyBangTestOptDescrs $
    \updates ->
      case foldM (flip ($)) defaultTinyBangTestOptions updates of
        Left msg -> do
          putStrLn msg
          exitWith $ ExitFailure $ -1
        Right options -> do
          -- First, configure logging instructions
          mconcat <$> mapM configureByInstruction (loggerInstructions options)
          configureLoggingHandlers
          -- Assertion options next
          when (staticAssertions options) $ do
            enableAssertions
            putStrLn "Assertions enabled!"
          -- Build the source file options
          let sfOpts =
                SourceFile.SourceFileTestConfig
                  { SourceFile.sftFilter = sourceFileOnlyByName options
                  , SourceFile.sftDatabase = emptyDatabase options
                  }
          -- Construct the appropriate source file tests
          let sfTests =
                sequence
                  [ testGroup "source file tests" <$> hUnitTestToTests <$>
                      SourceFile.generateTests sfOpts
                  ]
          -- Construct the other tests
          let otherTests = return
                  [ testGroup "NFA tests" $ hUnitTestToTests NFA.tests
                  , testGroup "Contour tests" $ hUnitTestToTests Contours.tests
                  ]
          -- Build the test list
          case SourceFile.sftFilter sfOpts of
            Nothing -> concat <$> sequence [otherTests, sfTests]
            Just _ -> sfTests
