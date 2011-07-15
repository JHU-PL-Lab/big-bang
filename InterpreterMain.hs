module Main where

import qualified Interpreter as I
import qualified Lexer as L
import qualified Parser as P

main :: IO ()
main = do
    inp <- getContents
    print $ I.evalTop $ P.parseBigBang $ L.lexBigBang inp
