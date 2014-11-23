{-|
  The primary test module for LittleBang.
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
import qualified Test.LittleBang.SourceFile as SourceFile
import Test.LittleBang.Syntax.Parser
import Test.Utils.TestFramework.Options

-- |The main for the LittleBang unit tests.
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
                SourceFile.LittleBangSourceFileTestConfig
                  { SourceFile.lbsftFilter = sourceFileOnlyByName options
                  , SourceFile.lbsftTypeSystem =
                      fromJust $ typeSystemImplementation options
                  }
          -- Construct the appropriate source file tests
          let sfTests =
                sequence
                  [ testGroup "source file tests" <$> hUnitTestToTests <$>
                      SourceFile.generateTests sfOpts
                  ]
          -- Construct the other tests
          -- TODO: other tests
          -- Build the test list
          case SourceFile.lbsftFilter sfOpts of
            Nothing -> concat <$> sequence
                        [ return
                            [ testGroup "parser tests" $
                                hUnitTestToTests parserTests
                            ]
                        , sfTests
                        ]
            Just _ -> sfTests
