{-# LANGUAGE TupleSections, ScopedTypeVariables, TemplateHaskell #-}

{-|
  This module implements a small-step interpreter for the TinyBang language.
-}

module Language.TinyBang.Interpreter
( eval
-- re-exports
, EvalEnv(..)
, EvalError(..)
, IllFormedness(..)
, EvalState(..)
) where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Interpreter.Builtins
import Language.TinyBang.Interpreter.Matching
import Language.TinyBang.Interpreter.Variables
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

-- |The small-step evaluation routine for TinyBang.  The result is an evaluation
--  environment (representing the heap) and a flow variable (representing the
--  result of evaluation).  If an error occurs, it is accompanied by the list
--  of unevaluated clauses when evaluation failed.
eval :: Expr -> Either (EvalError, [Clause]) (EvalEnv, Var)
eval (Expr o cls) =
  let e'@(Expr _ cls') = Expr o $ builtinEnv ++ cls in
  let initialState = EvalState ( EvalEnv Map.empty Nothing ) cls' 1 in
  let (merr, rstate) = runEvalM initialState $
                          wellFormednessCheck e' >> smallStepMany
  in
  either (Left . (,evalClauses rstate))
         (Right . const ( evalEnv rstate
                        , fromJust $ lastVar $ evalEnv rstate))
         merr
  where
    wellFormednessCheck :: Expr -> EvalM ()
    wellFormednessCheck e' =
      let illFormednesses = checkWellFormed e' in
      unless (Set.null illFormednesses) $
        raiseEvalError $ IllFormedExpression illFormednesses
    smallStepMany :: EvalM ()
    smallStepMany = do
      smallStep
      cs <- getClauses
      unless (null cs) smallStepMany

-- |A function to perform a single small step of evaluation.  This function will
--  attempt to consume one of the clauses in the environment.  It will either
--  produce a new environment (with one of the clauses consumed or otherwise
--  processed) or an error.  If the environment has no clauses, it is returned
--  verbatim.
smallStep :: EvalM ()
smallStep = do
  cls <- getClauses
  _infoI (display $ text "Clauses: " </> indent 2 (makeDoc cls)) $
    case cls of
      [] -> return ()
      RedexDef orig x2 redex : _ ->
        let orig' = ComputedOrigin [orig] in
        case redex of
          Define _ x1 -> do
            v' <- varLookup x1
            replaceFirstClause [Evaluated $ ValueDef orig' x2 v']
          Appl _ x0 x1 -> do
            me' <- matches x0 x1
            case me' of
              Nothing -> raiseEvalError $ ApplicationFailure x0 x1
              Just e' -> do
                e''@(Expr _ cls'') <- freshen e'
                x' <- rvexpr e''
                replaceFirstClause $
                  cls'' ++ [RedexDef orig x2 $ Define generated x']
          Builtin o bop xs -> do
            v <- evalBuiltin o bop xs
            replaceFirstClause [Evaluated $ ValueDef orig' x2 v]
      Evaluated ecl : _ ->
        case ecl of
          ValueDef _ x v -> do
            setVar x v
            setMostRecent x
            replaceFirstClause []
  where
    rvexpr :: Expr -> EvalM Var
    rvexpr (Expr o cls) =
      if null cls
        then raiseEvalError $ IllFormedExpression $ Set.singleton $
              EmptyExpression o
        else case last cls of
                RedexDef _ x _ -> return x
                Evaluated (ValueDef _ x _) -> return x
