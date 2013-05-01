{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

{-|
  A logging module for TinyBang.  This module uses the HSLogger package.  It
  provides convenience mechanisms for logging, such as module-specific loggic
  functions via TemplateHaskell.
-}

module Language.TinyBang.Logging
( loggingFunctions
) where

import Control.Applicative ((<$>),(*>))
import Language.Haskell.TH
import System.IO.Unsafe
import System.Log.Logger

import Language.TinyBang.Display

{-|
  A set of logging functions for the current module.  These logging functions
  are generated as follows:
    * Inline logging functions of the form @_debugI@ with type
      @String -> a -> a@.  The priority of the message is described
      by the function name; the logger's name is the same as the module's name.
      Similar functions exist for each logging level (e.g. @_warningI@).
    * Inline value logging functions of the form @_debugValI@ with type
      @(Display a) => String -> a -> a@.  In this case, the string is merely
      a message prefix; the logged message is that prefix followed by the
      display of the given value.
    * Monadic logging functions of the form @_debug@ with type
      @(Monad m) => String -> m ()@.
    * Monadic value logging functions of the form @_debugVal@ with type
      @(Monad m, Display a) => String -> a -> m()@.
-}
loggingFunctions :: Q [Dec]
loggingFunctions = do
  let levels = [ ("_debug", [|DEBUG|])
               , ("_info", [|INFO|])
               , ("_notice", [|NOTICE|])
               , ("_warning", [|WARNING|])
               , ("_error", [|ERROR|])
               , ("_critical", [|CRITICAL|])
               , ("_alert", [|ALERT|])
               , ("_emergency", [|EMERGENCY|])
               ]
  let generators = [ inlineLoggingFunction
                   , inlineValueLoggingFunction
                   , monadicLoggingFunction
                   , monadicValueLoggingFunction ]
  let generated = map (\f -> concat <$> mapM f levels) generators
  concat <$> sequence generated
  where
    moduleNameExpr :: Q Exp
    moduleNameExpr = LitE <$> StringL <$> loc_module <$> location
    msgName = mkName "msg"
    argName = mkName "arg"
    inlineLoggingFunction :: (String, Q Exp) -> Q [Dec]
    inlineLoggingFunction (namePart,prioExpr) = do
      let name = mkName $ namePart ++ "I"
      let signature = sigD name [t| forall a. String -> a -> a |]
      let body = [| unsafePerformIO $!
                      logM $moduleNameExpr $prioExpr $(varE msgName)
                      *> return $(varE argName) |]
      let decl = funD name [clause [varP msgName, varP argName]
                  (normalB body) []]
      sequence [signature, decl]
    inlineValueLoggingFunction :: (String, Q Exp) -> Q [Dec]
    inlineValueLoggingFunction (namePart,prioExpr) = do
      let name = mkName $ namePart ++ "ValI"
      let otherName = mkName $ namePart ++ "I"
      let signature = sigD name [t| forall a. (Display a) => String -> a -> a |]
      let body = [| $(varE otherName)
            ($(varE msgName) ++ display $(varE argName)) $(varE argName) |]
      let decl = funD name [clause [varP msgName, varP argName]
                  (normalB body) []]
      sequence [signature,decl]
    monadicLoggingFunction :: (String, Q Exp) -> Q [Dec]
    monadicLoggingFunction (namePart,prioExpr) = do
      let name = mkName namePart
      let signature = sigD name [t| (Monad m) => String -> m () |]
      let body = [| return $! unsafePerformIO $!
            logM $(moduleNameExpr) $(prioExpr) $(varE msgName)|]
      let decl = funD name [clause [varP msgName] (normalB body) []]
      sequence [signature,decl]
    monadicValueLoggingFunction :: (String, Q Exp) -> Q [Dec]
    monadicValueLoggingFunction (namePart, prioExpr) = do
      let name = mkName $ namePart ++ "Val"
      let otherName = mkName namePart
      let signature = sigD name
            [t| (Monad m, Display a) => String -> a -> m () |]
      let body = [| $(varE otherName)
            ($(varE msgName) ++ display $(varE argName)) |]
      let decl = funD name [clause [varP msgName, varP argName]
                  (normalB body) []]
      sequence [signature, decl]
