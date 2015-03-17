module Language.TinyBang.Namespace.Directories

where

import System.IO
import System.IO.Error
import System.Directory
import System.Environment

import Data.List
import Data.Either

import Control.Monad
import Control.Applicative

import Language.TinyBang.Ast

type Separator = Char
type ModuleReadError = String -- TODO a nicer type?

-- | Utility to split a string on a character
splitOn :: Separator -> String -> [String]
splitOn sep s = cons (case break (== sep) s of
                                (l, s') -> (l, case s' of
                                                []    -> []
                                                _:s'' -> splitOn sep s''))
  where
    cons ~(h,t) = h:t

-- | List of files on the path given the path name
listPath :: String -> IO [FilePath]
listPath pathName = ((splitOn ':' <$> getEnv pathName) >>= (return . flip (:))) <*> getCurrentDirectory

--modNameToPath :: String -> FilePath
--modNameToPath = ((flip (++)) ".lb") . (map (\x -> if x == '.' then '/' else x))
modNameToPath :: String -> ModuleName -> FilePath
modNameToPath suff (ModuleName _ ss) = (concat $ intersperse "/" ss) ++ suff

tryPath :: FilePath -> FilePath -> IO (Either IOError Handle)
tryPath modPath basePath = tryIOError (openFile (basePath ++ ('/' : modPath)) ReadMode)

--loadFirst :: String -> IO (Either ModuleReadError Handle)
loadFirst :: ModuleName -> String -> String -> IO (Either ModuleReadError Handle)
loadFirst modName suff p =
    let mod = modNameToPath suff modName in
    let path = listPath p in
    let openAttempts = join ((mapM $ tryPath mod) <$> path) in
    do
        v <- rights <$> openAttempts
        case v of
            [] -> return $ Left $ ("Module " ++ (show modName) ++ " could not be found.")
            _ -> (mapM hClose (tail v) >> return (Right $ head v))
