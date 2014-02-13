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

import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.Utils.Logger
import Utils.GetOpt

data TinyBangTestOptions =
  TinyBangTestOptions
  { loggerInstructions :: [LoggingInstruction]
  , sourceFileOnlyByName :: Maybe String
  , emptyDatabase :: SomeDisplayableConstraintDatabase
  , staticAssertions :: Bool
  }
  
defaultTinyBangTestOptions :: TinyBangTestOptions
defaultTinyBangTestOptions =
  TinyBangTestOptions
    { loggerInstructions = []
    , sourceFileOnlyByName = Nothing
    , emptyDatabase = SomeDisplayableConstraintDatabase
                        (CDb.empty :: IndexedConstraintDatabase)
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
          db <- case x of
                  "simple" -> return $ SomeDisplayableConstraintDatabase
                                (CDb.empty :: CDb.SimpleConstraintDatabase)
                  "indexed" -> return $ SomeDisplayableConstraintDatabase
                                (CDb.empty :: CDb.IndexedConstraintDatabase)
                  _ -> throwError $ "Unrecognized database type: " ++ x
          return $ r { emptyDatabase = db}) "IMPL") $
      "Selects the constraint database implementation.  This must be one of " ++
      "the following: simple [indexed]"
  , Option "A" ["ea","enable-assertions"]
      (NoArg $ \r -> return $ r { staticAssertions = True })
      "Enables debugging assertions."
  ]

