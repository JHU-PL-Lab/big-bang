module Utils.Toploop
( getSrcLine
, toploop
) where

import Control.Monad
import Data.List.Split
import System.IO

-- |Enters a toploop in which source code is read and executed.  Each expression
--  is delimited by two semicolons in the fashion of OCaml.  The caller must
--  provide a function which will evaluate each string of source into a string
--  result.
toploop :: (String -> IO String) -> IO ()
toploop eval = do
  inp <- getSrcLine
  eof <- hIsEOF stdin
  doEvalPrint inp
  when (not eof) (toploop eval)
  where
    doEvalPrint :: String -> IO ()
    doEvalPrint exprSrc = do
      putStrLn =<< eval exprSrc
      putStrLn "###"
      hFlush stdout

getSrcLine :: IO String
getSrcLine = do 
  c1 <- getChar
  if not (c1 == ';')
    then do
      l1 <- getSrcLine
      return (c1:l1)
    else do
      c2 <- getChar
      if not (c2 == ';')
        then do
          l2 <- getSrcLine
          return (c2:l2)
        else
          return ""
