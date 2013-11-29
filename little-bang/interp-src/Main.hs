module Main where

import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Language.LittleBang.Translator
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Display
import Language.TinyBang.Toploop

import Language.TinyBangNested.ATranslation.Translator

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
  do
    let transResult = performTranslation =<< convertToTBNExpr =<< desugarLittleBang =<< parseLittleBang interpContext =<< lexLittleBang "" input
    case transResult of 
      Left x -> return x
      Right _ -> do  
                   let interpretResult = stringyInterpretSource testConfig (render $ makeDoc transResult)
                   return $ "\nTranslation::\n" ++  display transResult ++ "\n\nEvaluation::\n" ++ interpretResult
      
      
      
      
      
      
      
      
      