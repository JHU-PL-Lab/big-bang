module Main where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Either.Combinators
import System.IO

import Paths_tiny_bang_nested (version)
import Data.Version (showVersion)

import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBangNested.Syntax.Parser
import Language.TinyBang.Utils.Syntax.Location
import Language.TinyBang.Utils.Assertions
import Language.TinyBang.Utils.Display
import Language.TinyBangNested.ATranslator
import Language.TinyBang.Toploop
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Ast as TB
import Utils.CLI.Args.Logging
import Utils.GetOpt
import Utils.TinyBang.Options
import Utils.Toploop

versionStr :: String
versionStr = "TinyBangNested Interpreter version " ++ showVersion version

interpretTBNSource :: InterpreterConfiguration -> String
                   -> EitherT InterpreterError IO InterpreterResult
interpretTBNSource interpConf src = do
  tokens <- hoistEither $ mapLeft LexerFailure $ lexTinyBangNested UnknownDocument src
  tbnAst <- hoistEither $ mapLeft ParserFailure $ parseTinyBangNested UnknownDocument tokens
  let tbAst = aTranslate tbnAst
  interpretAst interpConf tbAst

stringyInterpretTBNSource :: InterpreterConfiguration -> String -> IO String
stringyInterpretTBNSource interpConf exprSrc = do
  res <- runEitherT $ interpretTBNSource interpConf exprSrc
  case res of
    Left err -> return $ display err
    Right result -> return $ display result

tbnExprFromSource :: String -> EvalM TB.Expr
tbnExprFromSource src = do
  case lexTinyBangNested UnknownDocument src of
    Left s -> raiseEvalError $ LoadError s
    Right t -> case parseTinyBangNested UnknownDocument t of
      Left s -> raiseEvalError $ LoadError s
      Right ast -> return $ aTranslate ast

-- |Creates an evaluation routine for a single expression.  Requires an initial
--  configuration.
makeEval :: TinyBangOptions -> IO (String -> IO String)
makeEval opts = do
  let ts = typeSystemImplementation opts
  let config = InterpreterConfiguration
                    { evaluating = not $ noEval opts
                    , typeSystem = ts
                    , loadContext = LoadContext
                      { loadFn = tbnExprFromSource
                      , loadSuffix = ".tbn"
                      , loadPathName = "TBNPATH"
                      }
                    }
  return $ stringyInterpretTBNSource config

main :: IO ()
main = do
  opts <- updaterParse tinyBangOptionDescriptors tinyBangDefaultOptions
  
  mapM_ configureLoggingInstruction $ loggingInstructions opts
  configureLoggingHandlers
  
  when (assertions opts) $ do
    enableAssertions
    putStrLn "Assertions enabled!"
  
  if batchMode opts     
    then
      -- TODO: batch mode for TBN
      ioError $ userError "No batch mode yet implemented for TinyBangNested!"
    else do 
      putStrLn versionStr
      putStrLn ""
      putStrLn "###"
      hFlush stdout
  
      eval <- makeEval opts
      toploop eval
