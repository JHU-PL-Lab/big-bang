{-# LANGUAGE GADTs #-}

module Language.TinyBang.Interpreter.Equality
( varValueEq
) where

import Control.Applicative

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Interpreter.DeepValues

-- |Calculates equality for the contents of two variables.
varValueEq :: Var -> Var -> EvalM Bool
varValueEq x x' = do
  -- NOTE: This definition may change when cells are introduced if we decide
  --       that equality looks through cells.
  env <- varMap <$> getEnv
  return $ deepOnion env x == deepOnion env x'
