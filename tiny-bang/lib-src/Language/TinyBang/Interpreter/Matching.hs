{-# LANGUAGE TemplateHaskell #-}

module Language.TinyBang.Interpreter.Matching
( matches
) where

import Control.Applicative
import Data.Maybe

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Interpreter.Compatibility
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

-- |Computes the matching relation.  The first argument is the compound
--  function (scape) to use; the second argument is the argument.
matches :: Var -> Var -> EvalM (Maybe Expr)
matches x0 x1 =
  bracketLogM _debugI
    (display $ text "Checking matching of scape " <+> makeDoc x0 <+>
               text " with argument " <+> makeDoc x1)
    (\result ->
        case result of
          Just expr ->
            display $ text "Scape " <+> makeDoc x0 <+>
                      text " matches argument " <+> makeDoc x1 <+>
                      text " with body " <+> makeDoc expr
          Nothing ->
            display $ text "Scape " <+> makeDoc x0 <+>
                      text " does not match argument " <+> makeDoc x1
    ) $
    do
      v <- varLookup x0
      case v of
        VOnion _ x2 x3 -> do
          me1 <- matches x2 x1
          if isNothing me1 then matches x3 x1 else return me1
        VScape _ (Pattern _ pv pfm) e' -> do
          b <- compatibility x1 pv pfm
          return $ exprConcat <$> b <*> Just e'
        _ -> return Nothing
