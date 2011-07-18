module Main where

import qualified Language.BigBang.Interpreter.Interpreter as I
import qualified Language.BigBang.Syntax.Lexer as L
import qualified Language.BigBang.Syntax.Parser as P

main :: IO ()
main = do
    inp <- getContents
    print $ I.evalTop $ P.parseBigBang $ L.lexBigBang inp
