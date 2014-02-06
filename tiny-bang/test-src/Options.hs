{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TupleSections #-}

{-|
  This module handles command-line options for the TinyBang unit test system.
-}

module Options
( parseOptions
, TinyBangTestOptions(..)
) where

import Control.Applicative
import Data.Either
import Data.Either.Combinators
import Data.Monoid
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Log
import Test.Framework

import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.Utils.Logger

{-
  TO ADD A NEW OPTION, the following changes must be made:
    1. Add an entry to the TinyBangTestOptions record.
    2. Add a default Nothing value to the mempty implementation.
    3. Correctly implement mappend.
    4. Add an entry to the defaultOptions value.
    5. Add an entry to TinyBangTestOptions to allow it to be used on the command
       line.
-}

-- |Parses command line arguments for the TinyBang test system.
parseOptions :: [String]
             -> IO (Either (String,ExitCode)
                      (RunnerOptions, TinyBangTestOptions))
parseOptions args = do
  -- Set some simple defaults
  progName <- getProgName
  let usageHeader = "Usage: " ++ progName ++ " [OPTIONS]"
  -- Build a list of option descriptions for getOpt and run it
  let optDescrs = map (liftOptDescr $ Right . (,mempty)) optionsDescription
               ++ map (liftOptDescr ((Just mempty,) <$>)) tinyBangTestOptions
  let (results,nonOpts,errs) = getOpt Permute optDescrs args
  let extrasErr =
        if null nonOpts then [] else
              ["Unrecognized arguments: " ++ unwords nonOpts]
  -- Smash out the result as errors
  let (errs',parsedOptions) = partitionEithers results
  let errs'' = concat $ errs ++ map (++"\n") (errs' ++ extrasErr)
  -- Inline the defaults
  let (mtfOptList, tbtOptList) = unzip parsedOptions
  let mtfOpts = mconcat <$> sequence mtfOptList
  let tbtOpts = mconcat tbtOptList
  -- Now figure out what to do with them
  let usageStr = usageInfo usageHeader optDescrs
  case (errs'',mtfOpts) of
    (_,Nothing) ->
      return $ Left (usageStr, ExitSuccess)
    (_:_,_) ->
      return $ Left (errs'' ++ usageStr, ExitFailure 1)
    ([],Just tfOpts) ->
      -- We finally know that the option parsing worked out!  Now just attach
      -- the defaults and send them home.
      return $ Right (tfOpts, defaultOptions `mappend` tbtOpts)

-- |A wrapper for logger instructions.
type LoggerInstruction = (String,Priority)

-- |The record representing additional options parsed by this testing library.
--  Each value is a @Maybe@ to allow each record to describe part of the options
--  which have been set; @mappend@ then combines these options correctly.
data TinyBangTestOptions =
  TinyBangTestOptions
  { loggerInstructions :: Maybe [LoggerInstruction]
  , sourceFileOnlyByName :: Maybe (Maybe String)
  , emptyDatabase :: Maybe SomeDisplayableConstraintDatabase
  , staticAssertions :: Maybe Bool
  }
  
instance Monoid TinyBangTestOptions where
  mempty =
    -- Empty entries here
    TinyBangTestOptions
    { loggerInstructions = Nothing
    , sourceFileOnlyByName = Nothing
    , emptyDatabase = Nothing
    , staticAssertions = Nothing
    }
  mappend x y =
    -- Operations here to join two defined sets of behavior
    TinyBangTestOptions
    { loggerInstructions = mappendBy (++) loggerInstructions
    , sourceFileOnlyByName = mappendBy preferJustRight sourceFileOnlyByName
    , emptyDatabase = mappendBy (flip const) emptyDatabase
    , staticAssertions = mappendBy (flip const) staticAssertions
    }
    where
      mappendBy :: (a -> a -> a) -> (TinyBangTestOptions -> Maybe a) -> Maybe a
      mappendBy op prj =
        case (prj x, prj y) of
          (Just x', Just y') -> Just $ x' `op` y'
          (Nothing, Just y') -> Just y'
          (Just x', Nothing) -> Just x'
          (Nothing, Nothing) -> Nothing
      preferJustRight :: Maybe a -> Maybe a -> Maybe a
      preferJustRight a b =
        case (a,b) of
          (_, Just _) -> b
          _ -> a

-- |The default options for the TinyBang test system.  This record must contain
--  no @Nothing@ values; all top-level terms must be @Just@.
defaultOptions :: TinyBangTestOptions
defaultOptions =
  -- Defaults (when options are unspecified) go here
  TinyBangTestOptions
  { loggerInstructions = Just []
  , sourceFileOnlyByName = Just Nothing
  , emptyDatabase = Just $ SomeDisplayableConstraintDatabase
                      (CDb.empty :: IndexedConstraintDatabase)
  , staticAssertions = Just False
  }

-- |The getOpt descriptions for this program.
tinyBangTestOptions :: [OptDescr (Either String TinyBangTestOptions)]
tinyBangTestOptions =
  -- Each TinyBang test CLI option appears here as a getOpt description
  [ let parse s =
          case parseInstruction s of
            Left err -> Left err
            Right instr -> Right $ mempty { loggerInstructions = Just [instr] }
    in
    Option "L" ["log"] (ReqArg parse "log_cmd")
      "a logging instruction of the form PRIO or PRIO:MODULE (e.g. debug:Foo)"
  , let parse s = Right $ mempty {sourceFileOnlyByName = Just $ Just s } in 
    Option [] ["sf-only"] (ReqArg parse "filename")
      "execute only source file tests matching the provided filename"
  , let parse s =
          mapRight (\db -> mempty {emptyDatabase = Just db}) $
          case s of
            "simple" -> Right $ SomeDisplayableConstraintDatabase $
                          (CDb.empty :: SimpleConstraintDatabase)
            "indexed" -> Right $ SomeDisplayableConstraintDatabase $
                          (CDb.empty :: IndexedConstraintDatabase)
            _ -> Left $ "Unrecognized database name: " ++ s
    in
    Option [] ["db"] (ReqArg parse "impl")
      "execute tests using database implementation (one of: [simple] indexed)"
  , Option "A" ["ea", "enable-assertions"]
      (NoArg $ Right $ mempty {staticAssertions = Just True})
      "enable static assertions (must be compiled in)"
  ]
  
-- |Lifts an existing @OptDescr@ to a new space.
liftOptDescr :: (a -> b) -> OptDescr a -> OptDescr b
liftOptDescr f (Option short long argDescr descr) =
    let argDescr' = case argDescr of
                      NoArg x -> NoArg $ f x
                      ReqArg g name -> ReqArg (f . g) name
                      OptArg g name -> OptArg (f . g) name
    in Option short long argDescr' descr
