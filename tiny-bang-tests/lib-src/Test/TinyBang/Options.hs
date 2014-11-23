{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TupleSections #-}

{-|
  This module handles command-line options for the TinyBang unit test system.
-}

module Test.TinyBang.Options
( TinyBangTestOptions(..)
, defaultTinyBangTestOptions
, tinyBangTestOptDescrs
) where

import Control.Monad.Error
import System.Console.GetOpt

import Language.TinyBang.TypeSystem
import Language.TinyBang.Utils.Logger
import Utils.CLI.Args.Logging
import Utils.GetOpt

data TinyBangTestOptions =
  TinyBangTestOptions
  { loggerInstructions :: [LoggingInstruction]
  , sourceFileOnlyByName :: Maybe String
  , typeSystemImplementation :: Maybe TypeSystem
  , staticAssertions :: Bool
  }
  
defaultTinyBangTestOptions :: TinyBangTestOptions
defaultTinyBangTestOptions =
  TinyBangTestOptions
    { loggerInstructions = []
    , sourceFileOnlyByName = Nothing
    , typeSystemImplementation = Just simpleTypeSystem
    , staticAssertions = False
    }

tinyBangTestOptDescrs :: [OptDescr (OptionUpdater TinyBangTestOptions)]
tinyBangTestOptDescrs =
  [ Option "" ["log"]
      (ReqArg (\x r ->
        do
          instr <- parseInstruction x
          return $ r { loggerInstructions = instr : loggerInstructions r })
          "INSTR") $
      "Configures logging levels.  Each logging instruction is either a " ++
      "logging level (one of debug, info, notice, warning, error, or " ++
      "critical) or a Haskell module hierarchy name followed by a colon and " ++
      "a logging level."
  , Option "" ["sf-only"]
      (ReqArg (\x r ->
        case sourceFileOnlyByName r of
          Just _ -> throwError "Must only specify one source file option."
          Nothing -> return $ r { sourceFileOnlyByName = Just x }) "NAME") $
      "Only executes the test pertaining to a specific source file.  The " ++
      "provided argument is the name of the file without extension or the " ++
      "directory prefix."
  , Option "" ["db"]
      (ReqArg (\x r -> do
          ts <- case x of
                  "simple" -> return $ Just simpleTypeSystem
                  "indexed" -> return undefined -- TODO
                  _ -> throwError $ "Unrecognized database type: " ++ x
          return $ r { typeSystemImplementation = ts}) "IMPL") $
      "Selects the constraint database implementation.  This must be one of " ++
      "the following: simple [indexed]"
  , Option "A" ["ea","enable-assertions"]
      (NoArg $ \r -> return $ r { staticAssertions = True })
      "Enables debugging assertions."
  ]

