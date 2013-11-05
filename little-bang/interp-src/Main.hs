module Main where

import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBangNested.Syntax.Parser
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Display
import Language.TinyBangNested.ATranslation.Translator

import Language.TinyBang.Toploop
import Data.List.Split
import System.IO

import Paths_little_bang (version)
import Data.Version (showVersion)

main :: IO ()
main = do
  putStrLn $ "TinyBangNested interpreter version " ++ showVersion version
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
    let transResult = performTranslation =<< parseTinyBangNested interpContext =<< lexTinyBangNested "" input
    case transResult of 
      Left x -> return x
      Right _ -> do  
                   let interpretResult = stringyInterpretSource testConfig (render $ makeDoc transResult)
                   return $ "\nTranslation:\n" ++  display transResult ++ "\n\nEvaluation:\n" ++ interpretResult
