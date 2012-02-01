module Main where

{--
-- From TinyBang (for reference):

import Utils.Render.Display (display)
import qualified Language.MicroBang.Interpreter.SourceInterpreter as SI

import Data.List.Split

main :: IO ()
main = do
    inp <- getContents
    let xs = filter (not . null) $ splitOn "\n\n" inp
    mapM_ (putStrLn . display . SI.evalStringTop) xs
--}

-- TODO: implement
main :: IO ()
main = return ()

