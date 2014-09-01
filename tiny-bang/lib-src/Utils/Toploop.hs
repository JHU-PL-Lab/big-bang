module Utils.Toploop
( toploop
) where

import Data.List.Split
import System.IO

-- |Enters a toploop in which source code is read and executed.  Each expression
--  is delimited by two semicolons in the fashion of OCaml.  The caller must
--  provide a function which will evaluate each string of source into a string
--  result.
toploop :: (String -> IO String) -> IO ()
toploop eval = do
  inp <- getContents
  let exprSrcs = filter (not . null) $ splitOn ";;" inp
  mapM_ doEvalPrint exprSrcs
  where
    doEvalPrint :: String -> IO ()
    doEvalPrint exprSrc = do
      putStrLn =<< eval exprSrc
      putStrLn "###"
      hFlush stdout
