{-# LANGUAGE TupleSections #-}

{-|
  This module contains the parsing and configuration of CLI logging arguments
  for the interpreter.
-}

module Utils.CLI.Args.Logging
( configureLoggingByStrings
, parseInstruction
, parsePriority
, L.configureLoggingInstruction
, L.configureLoggingHandlers
) where

import Control.Applicative ((<$>))
import Data.List.Split
import System.Log
import System.Log.Logger

import Language.TinyBang.Utils.Logger as L

-- | Configures logging from a set of logging level strings.  These strings are
--   expected to be of the form "PRIO" or "PRIO:NAME" where PRIO is a logging
--   priority (one of debug, info, notice, warning, error, critical, alert, or
--   emergency) and NAME is the name of a module subtree.  Returns True if
--   configuration was successful; returns False if something went wrong.  If
--   an error occurs, a message is printed before False is returned.
configureLoggingByStrings :: [String] -> IO Bool
configureLoggingByStrings configs =
  case mapM parseInstruction configs of
    Left err -> do
      putStrLn $ "Logging configuration error: " ++ err
      return False
    Right steps -> do
      mapM_ configureLoggingInstruction steps
      return True

parseInstruction :: String -> Either String LoggingInstruction
parseInstruction str =
  let elems = splitOn ":" str in
  case elems of
    _:_:_:_ -> Left $ "Too many colons: " ++ str
    [] -> Left "Invalid logging configuration"
    [prioStr] ->
      (rootLoggerName,) <$> nameToPrio prioStr
    [name, prioStr] ->
      (name,) <$> nameToPrio prioStr
  where
    nameToPrio :: String -> Either String Priority
    nameToPrio prioStr =
      maybe (Left $ "Invalid priority: " ++ prioStr) Right $
        parsePriority prioStr

parsePriority :: String -> Maybe Priority
parsePriority prioStr =
  case prioStr of
    "debug" -> Just DEBUG
    "info" -> Just INFO
    "notice" -> Just NOTICE
    "warning" -> Just WARNING
    "error" -> Just ERROR
    "critical" -> Just CRITICAL
    "alert" -> Just ALERT
    "emergency" -> Just EMERGENCY
    _ -> Nothing
