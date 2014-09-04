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
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
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
eval :: Expr -> EitherT (EvalError, [Clause]) IO (EvalEnv, Var)
eval (Expr o cls) = do
  let e'@(Expr _ cls') = Expr o $ builtinEnv ++ cls
  let initialState = EvalState ( EvalEnv Map.empty Nothing ) cls' 1
  (merr, rstate) <- liftIO $ runEvalM initialState $
                          wellFormednessCheck e' >> smallStepMany
  case merr of
    Left err -> left (err,evalClauses rstate)
    Right _ -> return $ ( evalEnv rstate
                        , fromJust $ lastVar $ evalEnv rstate)
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
      Clause orig x2 redex : _ ->
        let orig' = ComputedOrigin [orig] in
        case redex of
          Def _ v -> do
            setVar x2 v
            setMostRecent x2
            replaceFirstClause []
          Copy _ x1 -> do
            v' <- varLookup x1
            replaceFirstClause [Clause orig' x2 $ Def orig' v']
          Appl _ x0 x1 -> do
            me' <- matches x0 x1
            case me' of
              Nothing -> raiseEvalError $ ApplicationFailure x0 x1
              Just e' -> do
                e''@(Expr _ cls'') <- freshen e'
                x' <- rvexpr e''
                replaceFirstClause $
                  cls'' ++ [Clause orig' x2 $ Copy orig' x']
          Builtin o bop xs -> do
            v <- evalBuiltin o bop xs
            replaceFirstClause [Clause orig' x2 $ Def orig' v]
          GetChar o -> do
            v <- returnTBChar o
            replaceFirstClause [Clause orig' x2 $ Def orig' v]
  where
    rvexpr :: Expr -> EvalM Var
    rvexpr (Expr o cls) =
      case reverse cls of
        [] -> raiseEvalError $ IllFormedExpression $ Set.singleton $
                EmptyExpression o
        Clause _ x _ : _ -> return x
