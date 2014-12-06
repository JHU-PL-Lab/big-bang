{-|
  The primary test module for TinyBang.
-}

module Main
( main
) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe
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
--  parse its options.  This is accomplished by the @defaultMainWithCustomOpts@
--  from @Test.Utils.TestFramework.Options@.
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
          mconcat <$> mapM configureLoggingInstruction (loggerInstructions options)
          configureLoggingHandlers
          -- Assertion options next
          when (staticAssertions options) $ do
            enableAssertions
            putStrLn "Assertions enabled!"
          -- Make sure we have a typechecker
          when (isNothing $ typeSystemImplementation options) $ do
            putStrLn "Cannot unit test without type system."
            exitWith $ ExitFailure $ -2
          -- Build the source file options
          let sfOpts =
                SourceFile.TinyBangSourceFileTestConfig
                  { SourceFile.tbsftFilter = sourceFileOnlyByName options
                  , SourceFile.tbsftTypeSystem =
                        fromJust $ typeSystemImplementation options
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
          case sourceFileOnlyByName options of
            Nothing -> concat <$> sequence [otherTests, sfTests]
            Just _ -> sfTests
