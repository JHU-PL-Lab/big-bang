{-# LANGUAGE ImplicitParams, DeriveDataTypeable #-}
module Main where

import Utils.Render.Display (display)
import qualified Language.TinyBang.Interpreter.SourceInterpreter as SI
import System.Console.CmdArgs

import Data.List.Split

data Options = Options
  { debug :: Bool
  } deriving (Data, Typeable, Show, Eq)

defOpts :: Options
defOpts = Options {debug = def &= name "d"}

main :: IO ()
main = do
    opts <- cmdArgs defOpts
    let ?debug = debug opts
    inp <- getContents
    let xs = filter (not . null) $ splitOn "\n\n" inp
    mapM_ (putStrLn . display . SI.evalStringTop) xs
