{-# LANGUAGE ImplicitParams #-}
module Language.TinyBang.Config
( Config(..)
, ConfigDebug
, ConfigTypecheck
, ConfigEvaluate
, debugging
, typechecking
, evaluating
, displayDebugging
) where

import Utils.Render.Display (ConfigDisplayDebug, isDisplayDebug)

data Config = Config { debug :: Bool
                     , typecheck :: Bool
                     , evaluate :: Bool
                     }

class ConfigDebug a where
    isDebug :: a -> Bool

class ConfigTypecheck a where
    isTypecheck :: a -> Bool

class ConfigEvaluate a where
    isEvaluate :: a -> Bool

debugging :: (ConfigDebug a, ?conf :: a) => Bool
debugging = isDebug ?conf

typechecking :: (ConfigTypecheck a, ?conf :: a) => Bool
typechecking = isTypecheck ?conf

evaluating :: (ConfigEvaluate a, ?conf :: a) => Bool
evaluating = isEvaluate ?conf

displayDebugging :: (ConfigDisplayDebug a, ?conf :: a) => Bool
displayDebugging = isDisplayDebug ?conf

instance ConfigDebug Config where
    isDebug = debug

instance ConfigTypecheck Config where
    isTypecheck = typecheck

instance ConfigEvaluate Config where
    isEvaluate = evaluate

instance ConfigDisplayDebug Config where
    isDisplayDebug = debug
