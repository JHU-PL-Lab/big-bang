{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

{-|
  This module defines a generic framework for unit tests based on a series of
  source files stored in a directory.
-}
module Test.TinyBang.TestUtils.SourceFileTest
( createSourceFileTests
, SourceFileTestConfig(..)
) where

import Control.Monad.Trans.Either
import Data.Either.Combinators
import System.Directory
import System.FilePath
import Test.HUnit

{-|
  Defines a data type for configuration of source file tests.  The fields are:
    * @sftDirectory@: A directory containing the source files.
    * @sftFilenameFilter@: A filename filter (from base filenames onto
      booleans).
    * @sftExpect@: A routine for generating an expectation from a source file
      (e.g. by reading a comment).
    * @sftExecute@ A routine for analyzing a source string given an expectation
      and producing a resulting test.
  The @sftExpect@ and @sftExecute@ routines are not obligated to tag their error
  messages with the provided filenames; they are given for reference purposes
  only.
-}
data SourceFileTestConfig
  = forall expectation.
    SourceFileTestConfig
      { sftDirectory :: FilePath
      , sftFilenameFilter :: FilePath -> Bool
      , sftExpect :: FilePath -> String -> Either String expectation
      , sftExecute :: FilePath -> String -> expectation -> EitherT String IO ()
      }

createSourceFileTests :: SourceFileTestConfig -> IO Test
createSourceFileTests
    (SourceFileTestConfig
      { sftDirectory = dir
      , sftFilenameFilter = fnFilter
      , sftExpect = getExpectation
      , sftExecute = execute
      }) = do
  dirContents <- getDirectoryContents dir
  let tests = map (makeTest . ((dir ++ [pathSeparator]) ++)) $
                  filter fnFilter dirContents
  return $ TestList tests
  where
    success :: IO ()
    success = return ()
    makeTest :: FilePath -> Test
    makeTest filename = TestLabel filename $ TestCase $ do
      source <- readFile filename
      let expectE = tag $ getExpectation filename source
      resultE <- runEitherT $ (execute filename source) =<< (hoistEither expectE) -- TODO: tag
      case resultE of
        Left msg ->
          assertFailure $ filename ++ ": " ++ msg
        Right () ->
          success
      where
        tag = mapLeft ((filename ++ ": ") ++ )
