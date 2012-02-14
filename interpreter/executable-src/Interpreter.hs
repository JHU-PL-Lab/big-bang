{-# LANGUAGE ImplicitParams, DeriveDataTypeable #-}
module Main where

import Utils.Render.Display (display)
import qualified Language.TinyBang.Interpreter.SourceInterpreter as SI
import System.Console.CmdArgs

import Data.List.Split

data Options = Options
  { debug :: Bool
  , noTypecheck :: Bool
  } deriving (Data, Typeable, Show, Eq)

defOpts :: Options
defOpts = Options { debug = def &= name "d"
                  , noTypecheck = def &= name "t"}

main :: IO ()
main = do
    opts <- cmdArgs defOpts
    let ?debug = debug opts
    let ?noTypecheck = noTypecheck opts
    inp <- getContents
    let xs = filter (not . null) $ splitOn "\n\n" inp
    mapM_ (putStrLn . display .
           if ?noTypecheck
             then SI.evalStringTopNoTypecheck
             else SI.evalStringTop) xs
