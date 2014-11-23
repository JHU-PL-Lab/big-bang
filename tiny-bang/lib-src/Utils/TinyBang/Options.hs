{-# LANGUAGE LambdaCase #-}

{-|
  A module defining the command-line option parsing behavior of the TinyBang
  interpreter.
-}
module Utils.TinyBang.Options
( TinyBangOptions(..)
, tinyBangDefaultOptions
, tinyBangOptionDescriptors
) where

import Control.Monad.Error
import Language.TinyBang.TypeSystem
import Language.TinyBang.Toploop
import Language.TinyBang.Utils.Logger
import Utils.GetOpt
import Utils.CLI.Args.Logging

data TinyBangOptions = TinyBangOptions
  { noEval :: Bool
  , loggingInstructions :: [LoggingInstruction]
  , typeSystemImplementation :: Maybe TypeSystem
  , batchMode :: Bool 
  , assertions :: Bool
  }

tinyBangDefaultOptions :: TinyBangOptions
tinyBangDefaultOptions =
  TinyBangOptions
    { noEval = False
    , loggingInstructions = []
    , typeSystemImplementation = Just simpleTypeSystem 
    , batchMode = False
    , assertions = False
    }

tinyBangOptionDescriptors :: [OptDescr (OptionUpdater TinyBangOptions)]
tinyBangOptionDescriptors =
  [ Option "T" ["no-typecheck"]
      (NoArg $ \r -> return $ r { typeSystemImplementation = Nothing })
      "Disables typechecking."
  , Option "E" ["no-eval"]
      (NoArg $ \r -> return $ r { noEval = True })
      "Disables evaluation."
  , Option "l" ["log"]
      (ReqArg (\x r -> do
          instr <- parseInstruction x
          return $ r { loggingInstructions = instr : loggingInstructions r })
        "CMD") $
      "Configures logging levels.  Each logging instruction is either a " ++
      "logging level (one of debug, info, notice, warning, error, or " ++
      "critical) or a Haskell module hierarchy name followed by a colon and " ++
      "a logging level."
  , Option "" ["db", "database"]
      (ReqArg (\x r -> do
          db <- case x of
                  "simple" -> return $ Just simpleTypeSystem
                  "indexed" -> return $ undefined -- TODO
                  _ -> throwError $ "Unrecognized type system: " ++ x
          return $ r { typeSystemImplementation = db}) "IMPL") $
      "Selects the constraint database implementation.  This must be one of " ++
      "the following: simple [indexed]"
  , Option "" ["batch-mode"]
      (NoArg $ \r -> return $ r { batchMode = True })
      "Enables batch mode operation."
  , Option "A" ["ea","enable-assertions"]
      (NoArg $ \r -> return $ r { assertions = True })
      "Enables debugging assertions."
  ]
