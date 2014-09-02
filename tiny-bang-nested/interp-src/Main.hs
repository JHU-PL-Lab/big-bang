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
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Utils.CLI.Args.Logging
import Utils.GetOpt
import Utils.TinyBang.Options
import Utils.Toploop

versionStr :: String
versionStr = "TinyBangNested Interpreter version " ++ showVersion version

interpretTBNSource :: (CDb.ConstraintDatabase db, Display db) => db -> InterpreterConfiguration -> String -> EitherT (InterpreterError db) IO (InterpreterResult)
interpretTBNSource dummy interpConf src = do
  tokens <- hoistEither $ mapLeft LexerFailure $ lexTinyBangNested UnknownDocument src
  tbnAst <- hoistEither $ mapLeft ParserFailure $ parseTinyBangNested UnknownDocument tokens
  let tbAst = aTranslate tbnAst
  interpretAst dummy interpConf tbAst

stringyInterpretTBNSource :: InterpreterConfiguration -> String -> IO String
stringyInterpretTBNSource interpConf exprSrc =
  case emptyDatabaseFromType $ databaseType interpConf of
    CDb.SomeDisplayableConstraintDatabase dummy -> do
      res <- runEitherT $ interpretTBNSource dummy interpConf exprSrc
      case res of
        Left err -> return $ display err
        Right result -> return $ display result

-- |Creates an evaluation routine for a single expression.  Requires an initial
--  configuration.
makeEval :: TinyBangOptions -> IO (String -> IO String)
makeEval opts = do
  let dtype = databaseConfigType opts
  let config = InterpreterConfiguration
                    { typechecking = not $ noTypecheck opts
                    , evaluating = not $ noEval opts
                    , databaseType = dtype }
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
    then do 
      -- TODO: batch mode for TBN
      ioError $ userError "No batch mode yet implemented for TinyBangNested!"
    else do 
      putStrLn versionStr
      putStrLn ""
      putStrLn "###"
      hFlush stdout
  
      eval <- makeEval opts
      toploop eval
