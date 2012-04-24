{-# LANGUAGE ImplicitParams, DeriveDataTypeable #-}
module Main where

import Data.List.Split
import System.Console.CmdArgs

import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.SourceInterpreter as SI
import Utils.Render.Display (display)

data Options = Options
  { debug :: Bool
  , noTypecheck :: Bool
  , noEval :: Bool
  } deriving (Data, Typeable, Show, Eq)

defOpts :: Options
defOpts = Options { debug = def &= name "d"
                  , noTypecheck = def &= name "T"
                  , noEval = def &= name "E" }

main :: IO ()
main = do
    opts <- cmdArgs defOpts
    let ?conf = Cfg.Config { Cfg.debug = debug opts
                           , Cfg.typecheck = not $ noTypecheck opts
                           , Cfg.evaluate = not $ noEval opts
                           }
    inp <- getContents
    let xs = filter (not . null) $ splitOn "\n\n" inp
    mapM_ (putStrLn . display . SI.evalStringTop) xs

