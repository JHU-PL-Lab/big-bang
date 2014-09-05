{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Either.Combinators
import System.IO

import Paths_little_bang (version)
import Data.Version (showVersion)

import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Language.LittleBang.TBNConversion
import Language.LittleBang.Translator
import Language.TinyBang.Utils.Syntax.Location
import Language.TinyBang.Toploop
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.Utils.Assertions
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger (loggingFunctions, postLog)
import Language.TinyBangNested.ATranslator
import Utils.CLI.Args.Logging
import Utils.GetOpt
import Utils.TinyBang.Options
import Utils.Toploop

$(loggingFunctions)

versionStr :: String
versionStr = "LittleBang Interpreter version " ++ showVersion version

-- |Creates an evaluation routine for a single expression.  Requires an initial
--  configuration.
interpretLBSource :: (CDb.ConstraintDatabase db, Display db) => db -> InterpreterConfiguration -> String -> EitherT (InterpreterError db) IO (InterpreterResult)
interpretLBSource dummy interpConf src = do
  tokens <- hoistEither $ postLog _debugI
    ( \tokens -> display $
        text "LittleBang tokens:" <+> makeDoc tokens ) $
    mapLeft LexerFailure $ lexLittleBang UnknownDocument src
  lbAst <- hoistEither $ postLog _debugI
    ( \lbAst -> display $
        text "Parsed LittleBang AST:" <> lineNest lbAst ) $
    mapLeft ParserFailure $ parseLittleBang UnknownDocument tokens
  dlbAst <- hoistEither $ postLog _debugI
    ( \dlbAst -> display $
        text "Desugared LittleBang AST:" <> lineNest dlbAst ) $
    mapLeft OtherFailure $ desugarLittleBang lbAst
  tbnAst <- hoistEither $ postLog _debugI
    ( \tbnAst -> display $
        text "Converted to TBN AST:" <> lineNest tbnAst ) $
    mapLeft OtherFailure $ convertToTBNExpr dlbAst
  let tbAst = postLog _debugI ( \tbAst' -> display $ text "A-translated AST:" <> lineNest tbAst' ) (aTranslate tbnAst)
  interpretAst dummy interpConf tbAst
  where
  lineNest x = line <> indent 2 (align $ makeDoc x)

stringyInterpretTBNSource :: InterpreterConfiguration -> String -> IO String
stringyInterpretTBNSource interpConf exprSrc =
  case emptyDatabaseFromType $ databaseType interpConf of
    CDb.SomeDisplayableConstraintDatabase dummy -> do
      res <- runEitherT $ interpretLBSource dummy interpConf exprSrc
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
      -- TODO: batch mode for LB
      ioError $ userError "No batch mode yet implemented for LittleBang!"
    else do 
      putStrLn versionStr
      putStrLn ""
      putStrLn "###"
      hFlush stdout
  
      eval <- makeEval opts
      toploop eval

