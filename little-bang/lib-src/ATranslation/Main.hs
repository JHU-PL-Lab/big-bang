module Main where

import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBangNested.Syntax.Parser
import Language.TinyBang.Syntax.Location
import Language.TinyBangNested.Ast.Data
import Language.TinyBang.Display
import ATranslation.Translator
import Language.TinyBang.Toploop
import Data.List.Split
import System.IO


main :: IO ()
main = do
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

extractRight :: Either a b -> b
extractRight (Left _) = error "left error!"
extractRight (Right x ) = x

testConfig :: InterpreterConfiguration
testConfig = InterpreterConfiguration True True Simple

interpName :: String
interpName = "Interpreter"

interpContext :: ParserContext
interpContext = ParserContext UnknownDocument "Interpreter"


-- | Function for automating calls to lexTinyBangNested and unwrapping result
getLexerResult :: String -> [PositionalToken]
getLexerResult input =  extractRight $ lexTinyBangNested "" input

-- | Function for automating calls to parseTinyBangNested and displaying result
getParserResult :: [PositionalToken] -> Expr
getParserResult input =  extractRight $ parseTinyBangNested interpContext input


-- | Wrapper for evaluation
eval :: String -> IO String
eval input = return $ stringyInterpretSource testConfig (render $ makeDoc $ performTranslation $ getParserResult $ getLexerResult input)

