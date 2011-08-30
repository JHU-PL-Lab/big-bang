module Main where

import Language.BigBang.Render.Display (display)
import qualified Language.BigBang.Interpreter.SourceInterpreter as SI

import Data.List.Split

main :: IO ()
main = do
    inp <- getContents
    let xs = filter (not . null) $ splitOn "\n\n" inp
    mapM_ (putStrLn . display . SI.evalStringTop) xs
