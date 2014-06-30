{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
  This module contains Template Haskell utilities which will produce
  module-specific logging functions.
-}
module Language.TinyBang.Utils.Logger.Generators
( loggingFunctions
) where

import Control.Applicative
import Language.Haskell.TH
import System.Log

import Language.TinyBang.Utils.Logger.Operations

{-|
  A set of logging functions for the current module.  These logging functions
  are generated as follows:
    * Inline logging functions of the form @_debugI@ with type
      @String -> a -> a@.  The priority of the message is described
      by the function name; the logger's name is the same as the module's name.
      Similar functions exist for each logging level (e.g. @_warningI@).
    * Monadic logging functions of the form @_debugM@ with type
      @(Monad m) => String -> m ()@.
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
  let modes = [ ("I", [|logI|],
                  [t| forall a. String -> a -> a |])
              , ("M", [|logM|],
                  [t| forall m. (Monad m) => String -> m () |])
              ]
  concat <$> sequence [ loggingFunction mode level
                      | mode <- modes
                      , level <- levels
                      ]
  where
    moduleNameExpr :: Q Exp
    moduleNameExpr = LitE <$> StringL <$> loc_module <$> location
    
    loggingFunction :: (String,Q Exp,Q Type) -> (String, Q Exp) -> Q [Dec]
    loggingFunction (nameSuffix,baseFn,typ) (namePart,prioExpr) = do
      let fnName    = mkName $ namePart ++ nameSuffix
      let logName   = moduleNameExpr
      let signature = sigD fnName typ
      let decl      = funD fnName [clause [] (normalB [| $(baseFn) $(logName) $(prioExpr) |]) []]
      sequence [signature, decl]
