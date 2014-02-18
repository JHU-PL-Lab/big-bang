{-# LANGUAGE ScopedTypeVariables #-}

{-|
  Additional utilities for @System.Console.GetOpt@ which make higher-order
  operations somewhat simpler.
-}
module Utils.GetOpt
( OptionUpdater

, mapOptDescr
, mapArgDescr
, mergeOpts
, mapUpdaterOptDescr

, updaterParse
, updaterParsePure

, module System.Console.GetOpt
) where

import Control.Monad.Error
import Data.Either.Combinators
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit

type OptionUpdater a = (a -> Either String a)

mapOptDescr :: (a -> b) -> OptDescr a -> OptDescr b
mapOptDescr f (Option short long argDescr help) =
  Option short long (mapArgDescr f argDescr) help
  
mapArgDescr :: (a -> b) -> ArgDescr a -> ArgDescr b
mapArgDescr f descr = case descr of
  NoArg x -> NoArg $ f x
  ReqArg g name -> ReqArg (f . g) name
  OptArg g name -> OptArg (f . g) name

mergeOpts :: [OptDescr a] -> [OptDescr b] -> [OptDescr (Either a b)]
mergeOpts as bs = map (mapOptDescr Left) as ++ map (mapOptDescr Right) bs

mapUpdaterOptDescr :: (a -> b) -> (a -> b -> a) -> OptDescr (OptionUpdater b) -> OptDescr (OptionUpdater a)
mapUpdaterOptDescr extract replace =
  mapOptDescr (\f x -> mapRight (replace x) $ f $ extract x)

-- |Parses the command-line arguments using a parser for option updaters.  If
--  the parse fails, the program halts.
updaterParse :: [OptDescr (OptionUpdater a)]
             -> a
             -> IO a
updaterParse opts defaults = do
  args <- getArgs
  case updaterParsePure args opts defaults of
    Left msg -> putStrLn msg >> exitWith (ExitFailure $ -1)
    Right x -> return x

-- |Parses the provided arguments using a parser for option updaters.
updaterParsePure :: forall a. [String]
                 -> [OptDescr (OptionUpdater a)]
                 -> a
                 -> Either String a
updaterParsePure args opts defaults =
  let (out :: [a -> Either String a], nonOptArgs, errs) = getOpt RequireOrder opts args in
  case () of
    _ | not $ null errs ->
          throwError $ usageInfo (intercalate "," errs) opts
    _ | not $ null nonOptArgs ->
          throwError $ usageInfo ("Extra trailing args: " ++
                                      unwords nonOptArgs) opts
    _ -> case foldM (flip ($)) defaults out of
            Left err -> throwError $ usageInfo err opts
            Right ans -> return ans
