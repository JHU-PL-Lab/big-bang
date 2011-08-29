module Main where

import qualified Language.LittleBang.Interpreter.Interpreter as I
import qualified Language.LittleBang.Syntax.Lexer as L
import qualified Language.LittleBang.Syntax.Parser as P

import Data.List.Split

main :: IO ()
main = do
    inp <- getContents
    let xs = filter (not . null) $ splitOn "\n\n" inp
    mapM_ (print . I.evalTop . P.parseLittleBang . L.lexLittleBang) xs
