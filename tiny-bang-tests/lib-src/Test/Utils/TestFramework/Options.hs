module Test.Utils.TestFramework.Options
( defaultMainWithCustomOpts
, interpretArgsWithExtensions
) where

import Control.Applicative
import Data.Either
import Data.List
import Data.Monoid
import System.Console.GetOpt
import System.Environment
import System.Exit
import Test.Framework.Providers.API
import Test.Framework.Runners.Console
import Test.Framework.Runners.Options
import Utils.GetOpt

-- |Acts as a main for a test routine which generates a series of tests based
--  on some custom command-line parameters.
defaultMainWithCustomOpts :: [OptDescr a] -> ([a] -> IO [Test]) -> IO ()
defaultMainWithCustomOpts descrs testsFn = do
  args <- getArgs
  (tfOptions, customOptions) <-
      case interpretArgsWithExtensions args descrs of
        Left errFn -> do
                        progName <- getProgName
                        putStrLn $ errFn progName
                        exitWith $ ExitFailure $ -1
        Right x -> return x
  tests <- testsFn customOptions
  defaultMainWithOpts tests tfOptions

-- |Similar to 'interpretArgs' from the @Test.Framework.Runners.Console@ package
--  but allows additional 'OptDescr' from @System.Console.GetOpt@ to be
--  provided.  On success, the returned tuple contains the parsed options.  On
--  failure, a function is returned which, given the name of the program, will
--  return a suitable usage explanation.
interpretArgsWithExtensions :: [String]
                            -> [OptDescr a]
                            -> Either (String -> String)
                                      (RunnerOptions, [a])
interpretArgsWithExtensions args descrs =
  let (results, extras, errors) = getOpt RequireOrder opts args in
  case () of
    _ | not $ null errors ->
          Left $ \progName ->
                    concat (intersperse "," errors) ++ "\n\n" ++ errFn progName
    _ | not $ null extras ->
          Left $ \progName ->
                    "Unrecognized parameters: " ++
                      concat (intersperse " " errors) ++ "\n\n" ++
                      errFn progName
    _ -> let (tfOptions, customOptions) = partitionEithers results in
         case mconcat <$> sequence tfOptions of
            Nothing -> Left $ errFn
            Just finishedOpts ->
              Right (finishedOpts, customOptions)
  where
    opts = mergeOpts optionsDescription descrs
    errFn progName =
      let usageHeader = "Usage: " ++ progName ++ " [OPTIONS]" in
      usageInfo usageHeader opts
