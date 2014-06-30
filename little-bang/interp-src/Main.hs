{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Data.Either.Combinators
import System.IO

import Paths_little_bang (version)
import Data.Version (showVersion)

import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Language.LittleBang.TBNConversion
import Language.LittleBang.Translator
import Language.TinyBang.Syntax.Location
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
makeEval :: TinyBangOptions -> IO (String -> IO String)
makeEval opts = do
  let dtype = databaseConfigType opts
  let config = InterpreterConfiguration
                    { typechecking = not $ noTypecheck opts
                    , evaluating = not $ noEval opts
                    , databaseType = dtype }
  return $ \src ->
    let result =
          do -- Either
            tokens <-
              postLog _debugI
                ( \tokens -> display $
                    text "LittleBang tokens:" <+> makeDoc tokens)
                $ lexLittleBang UnknownDocument src
            lbAst <-
              postLog _debugI
                ( \lbAst -> display $
                    text "Parsed LittleBang AST:" <> lineNest lbAst)
                $ parseLittleBang UnknownDocument tokens
            dlbAst <-
              postLog _debugI
                ( \dlbAst -> display $
                    text "Desugared LittleBang AST:" <> lineNest dlbAst)
                $ desugarLittleBang lbAst
            tbnAst <-
              postLog _debugI
                ( \tbnAst -> display $
                    text "Converted to TBN AST:" <> lineNest tbnAst)
                $ convertToTBNExpr dlbAst 
            let tbAst =
                  postLog _debugI
                    (\tbAst' -> display $
                        text "A-translated AST:" <> lineNest tbAst')
                    $ aTranslate tbnAst
            case emptyDatabaseFromType dtype of
              SomeDisplayableConstraintDatabase db ->
                mapLeft display $ interpretAst db config tbAst
    in
    return $ either id display result
  where
    lineNest x = line <> indent 2 (align $ makeDoc x)
    
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

