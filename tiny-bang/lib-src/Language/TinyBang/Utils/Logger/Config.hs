{-# LANGUAGE TupleSections #-}

{-|
  This module contains the logging configuration functions for TinyBang's
  logging system.
-}
module Language.TinyBang.Utils.Logger.Config
( configureLoggingHandlers
, configureLoggingInstruction
, LoggingInstruction
) where

import Data.List
import Data.List.Split
import System.Log
import System.Log.Handler.Simple
import System.Log.Logger

type LoggingInstruction = (String, Priority)
      
-- | Given a module name and a priority, sets that module to log only messages
--   of that priority and higher.
configureLoggingInstruction :: LoggingInstruction -> IO ()
configureLoggingInstruction (loggerName, prio) =
  updateGlobalLogger loggerName $ setLevel prio
  
-- | Configures initial logging handlers.  Should be called before any logging
--   occurs.
configureLoggingHandlers :: IO ()
configureLoggingHandlers =
  updateGlobalLogger rootLoggerName $ setHandlers [handler]
  where
    handler = GenericHandler
      { priority = DEBUG
      , privData = ()
      , writeFunc = const putStrLn
      , closeFunc = const $ return ()
      , formatter = format --simpleLogFormatter "($prio:$loggername): $msg"
      }
    format :: a -> (Priority,String) -> String -> IO String
    format = \_ (prio,msg) name ->
      return $
        "(" ++ show prio ++ ":" ++ name ++ "):\n" ++
        (concat $ intersperse "\n" $ map ("    "++) $ splitOn "\n" msg)
