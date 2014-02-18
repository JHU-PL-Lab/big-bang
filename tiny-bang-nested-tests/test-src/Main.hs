{-|
  The primary test module for TinyBangNested.
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
import qualified Test.TinyBangNested.SourceFile as SourceFile
import Test.TinyBangNested.Syntax.Lexer
import Test.TinyBangNested.Syntax.Parser
import Test.Utils.TestFramework.Options

-- |The main for the TinyBangNested unit tests.
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
                SourceFile.TinyBangNestedSourceFileTestConfig
                  { SourceFile.tbnsftFilter = sourceFileOnlyByName options
                  , SourceFile.tbnsftDatabase = emptyDatabase options
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
          case SourceFile.tbnsftFilter sfOpts of
            Nothing -> concat <$> sequence
                        [ return
                            [ testGroup "lexer tests" $
                                hUnitTestToTests lexerTests
                            , testGroup "parser tests" $
                                hUnitTestToTests parserTests
                            ]
                        , sfTests
                        ]
            Just _ -> sfTests
