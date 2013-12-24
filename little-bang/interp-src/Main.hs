module Main where

import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Language.LittleBang.TBNConversion
import Language.LittleBang.Translator
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Display
import Language.TinyBang.Logging
import Language.TinyBang.Toploop
import Utils.Toploop.Logging

import Language.TinyBangNested.ATranslation.Translator

import Control.Monad
import Data.Either.Combinators
import Data.List.Split
import System.IO

import Paths_little_bang (version)
import Data.Version (showVersion)

main :: IO ()
main = do
  putStrLn $ "LittleBang interpreter version " ++ showVersion version
  putStrLn ""
  putStrLn "###"
  inp <- getContents
  let exprSrcs = filter (not . null) $ splitOn ";;" inp
  mapM_ doEvalPrint exprSrcs
  where
    doEvalPrint :: String -> IO ()
    doEvalPrint exprSrc = do
      putStrLn =<< eval exprSrc
      putStrLn "###"
      hFlush stdout

-- | Configuration

testConfig :: InterpreterConfiguration
testConfig = InterpreterConfiguration True True Simple

interpName :: String
interpName = "Interpreter"

interpContext :: ParserContext
interpContext = ParserContext UnknownDocument "Interpreter"

-- | Wrapper for evaluation
eval :: String -> IO String
eval input =
 -- configureLogging ["debug"] >>
  let answer =
          do -- Either String String
            tokens <- lexLittleBang "" input
            lbAst <- parseLittleBang interpContext tokens
            lbAst' <- mapLeft show $ desugarLittleBang lbAst
            tbnAst <- convertToTBNExpr lbAst'
            tbAst <- mapLeft show $ performTranslation tbnAst
            let interpretResult = stringyInterpretSource testConfig (render $ makeDoc tbAst)
            return $ "\nDesugaring::\n" ++ display tbnAst ++
                     "\n\nTranslation::\n" ++  display tbAst ++
                     "\n\nEvaluation::\n" ++ interpretResult
  in return $ either id id answer
  
{-     
    let transResult = performTranslation =<< convertToTBNExpr =<< desugarLittleBang =<< parseLittleBang interpContext =<< lexLittleBang "" input
    case transResult of 
      Left x -> return x
      Right _ -> do  
                   let interpretResult = stringyInterpretSource testConfig (render $ makeDoc transResult)
                   return $ "\nTranslation::\n" ++  display transResult ++ "\n\nEvaluation::\n" ++ interpretResult
-}      
      
      
      
      
      
      
      
      