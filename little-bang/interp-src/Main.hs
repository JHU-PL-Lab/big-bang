module Main where

import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Language.LittleBang.TBNConversion
import Language.LittleBang.Translator
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Utils.Display
import Language.TinyBang.Toploop

import Language.TinyBangNested.ATranslator.Translator

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
interpContext = ParserContext UnknownDocument

-- | Wrapper for evaluation
eval :: String -> IO String
eval input = 
  do
    let convResult = convertToTBNExpr =<< desugarLittleBang =<< parseLittleBangNested UnknownDocument =<< lexLittleBang UnknownDocument input
    case convResult of 
      Left x -> return x
      Right y -> do  
                   let interpretResult = stringyInterpretSource testConfig (render $ makeDoc $ aTranslate y)
                   return $ "\nTranslation::\n" ++  display y ++ "\n\nEvaluation::\n" ++ interpretResult
      
      
      
      
      
      
      
      
      
