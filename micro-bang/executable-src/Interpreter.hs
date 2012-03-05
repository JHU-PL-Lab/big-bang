{-# LANGUAGE ImplicitParams #-}
module Main where
import Utils.Render.Display (display)

{--
-- From TinyBang (for reference):


import qualified Language.MicroBang.Interpreter.SourceInterpreter as SI

import Data.List.Split

main :: IO ()
main = do
    inp <- getContents
    let xs = filter (not . null) $ splitOn "\n\n" inp
    mapM_ (putStrLn . display . SI.evalStringTop) xs
--}

-- TODO: implement
--main :: IO ()
--main = return ()
import qualified Language.MicroBang.Interpreter.SourceInterpreter as SI

import Data.List.Split

main :: IO ()
main = do
    let ?debug = False
    inp <- getContents
    let xs = filter (not . null) $ splitOn "\n\n" inp
    mapM_ (putStrLn . display . SI.evalStringTop) xs

