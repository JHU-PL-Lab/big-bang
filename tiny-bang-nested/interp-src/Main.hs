module Main where

import Control.Monad
import Data.Either.Combinators
import System.IO

import Paths_tiny_bang_nested (version)
import Data.Version (showVersion)

import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBangNested.Syntax.Parser
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Utils.Assertions
import Language.TinyBang.Utils.Display
import Language.TinyBangNested.ATranslator
import Language.TinyBang.Toploop
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Utils.GetOpt
import Utils.TinyBang.Options
import Utils.Toploop
import Utils.Toploop.Logging

versionStr :: String
versionStr = "TinyBangNested Interpreter version " ++ showVersion version

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
          do
            tokens <- lexTinyBangNested UnknownDocument src
            tbnAst <- parseTinyBangNested UnknownDocument tokens
            let tbAst = aTranslate tbnAst
            case emptyDatabaseFromType dtype of
              SomeDisplayableConstraintDatabase db ->
                mapLeft display $ interpretAst db config tbAst
    in
    return $ either id display result
    
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
