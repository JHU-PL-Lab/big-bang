module Main where

import qualified Language.BigBang.Interpreter.Interpreter as I
import qualified Language.BigBang.Syntax.Lexer as L
import qualified Language.BigBang.Syntax.Parser as P

import Data.List.Split

main :: IO ()
main = do
    inp <- getContents
    let xs = filter (not . null) $ splitOn "\n\n" inp
    mapM_ (print . I.evalTop . P.parseBigBang . L.lexBigBang) xs
