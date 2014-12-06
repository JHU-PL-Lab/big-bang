module Main where

import Paths_tiny_bang (version) -- Generated by Cabal
import Data.Version (showVersion)

import Control.Monad
import System.IO

import Language.TinyBang.Toploop
import Language.TinyBang.Utils.Assertions
import Utils.GetOpt
import Utils.TinyBang.Options
import Utils.Toploop
import Utils.CLI.Args.Logging

--import Lanugage.TinyBang.ToploopBM
--import Language.TinyBang.Communicator
--import Language.TinyBang.Communnicator.FromHaskellObject

versionStr :: String
versionStr = "TinyBang Interpreter version " ++ showVersion version

-- |Creates an evaluation routine for a single expression.  Requires an initial
--  configuration.
makeEval :: TinyBangOptions -> IO (String -> IO String)
makeEval opts = do
  let config = InterpreterConfiguration
                    { evaluating = not $ noEval opts
                    , typeSystem = typeSystemImplementation opts
                    }
  return $ stringyInterpretSource config

-- |Executes the TinyBang interpreter.
main :: IO ()
main = do
  opts <- updaterParse tinyBangOptionDescriptors tinyBangDefaultOptions
  
  mapM_ configureLoggingInstruction $ loggingInstructions opts
  configureLoggingHandlers
  
  when (assertions opts) $ do
    enableAssertions
    putStrLn "Assertions enabled!"
  
  if batchMode opts     
    then do
      -- |Method for batchMode
      let config = InterpreterConfiguration
                    { evaluating = not $ noEval opts
                    , typeSystem = typeSystemImplementation opts
                    }
      batchLoop config

    else do 
      putStrLn versionStr
      putStrLn ""
      putStrLn "###"
      hFlush stdout
  
      eval <- makeEval opts
      toploop eval
    where
    consumeChar :: IO ()
    consumeChar = do
      _ <- getChar
      return ()
    batchLoop :: InterpreterConfiguration -> IO ()
    batchLoop config = do
      inp <- getSrcLine
      nc <- hLookAhead stdin
      eof <- hIsEOF stdin
      when (nc == '\n') $ do consumeChar -- flush it
      if (nc == '\n' || eof) -- TODO: this is a temporary approach
        then do
          ln <- stringyInterpretSource config inp
          putStrLn ln      
          when (not eof) (batchLoop config)
        else do
          putStrLn "Invalid character after ;;. After ;; there should be line feed or end of file."
          hFlush stdout
