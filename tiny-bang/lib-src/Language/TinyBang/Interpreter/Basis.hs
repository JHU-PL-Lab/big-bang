{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Language.TinyBang.Interpreter.Basis
( EvalEnv(..)
, EvalState(..)
, EvalError(..)
, IllFormedness(..)
, EvalM(..)
, runEvalM
, varLookup
, setVar
, setMostRecent
, getEnv
, raiseEvalError
, getClauses
, setClauses
, replaceFirstClause
, freshVar
) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Display

-- |A data structure containing an evaluation environment.
data EvalEnv = EvalEnv
                { varMap :: Map Var Value
                  -- ^ A map describing variable bindings
                , lastVar :: Maybe Var
                  -- ^ The most recent evaluated variable (or @Nothing@ when
                  --   evaluation has not yet started).
                }
  deriving (Eq, Ord, Show)
                
-- |A data strcture representing evaluation state.
data EvalState = EvalState
                { evalEnv :: EvalEnv
                  -- ^ The current evaluation environment
                , evalClauses :: [Clause]
                  -- ^ The current evaluation clauses
                , evalVarIndex :: Integer
                  -- ^ The index of the next fresh varaible
                }
  deriving (Eq, Ord, Show)

-- |A data structure representing evaluation errors.
data EvalError
  = IllFormedExpression (Set IllFormedness)
  | ApplicationFailure Var Var
      -- ^Generated when pattern matching fails completely during application.
  deriving (Eq, Ord, Show)

-- |A data structure representing types of ill-formed expressions.
data IllFormedness
  = DuplicateDefinition Var
      -- ^Generated when a variable is declared twice.
  | OpenExpression (Set Var)
      -- ^Generated when the expression is not closed.
  | EmptyExpression Origin
      -- ^Generated when an empty expression (an expression with no clauses) is
      --  encountered
  | EmptyPattern Origin
      -- ^Generated when an empty pattern (a pattern with no clauses) is
      --  encountered.
  deriving (Eq, Ord, Show)

instance Display EvalError where
  makeDoc err = text $ show err

-- |The monad in which small-step evaluation takes place.
newtype EvalM a
  = EvalM { unEvalM :: EitherT EvalError (State EvalState) a }
  deriving (Functor, Applicative, Monad, MonadState EvalState)

-- |Executes an evaluation.
runEvalM :: EvalState -> EvalM a -> (Either EvalError a, EvalState)
runEvalM s x =
  runState (runEitherT $ unEvalM x) s

-- |Performs a value lookup on a given variable.
varLookup :: Var -> EvalM Value
varLookup x = do
  env <- evalEnv <$> get
  let mval = Map.lookup x $ varMap env
  fromMaybe (raiseEvalError $ IllFormedExpression $ Set.singleton $
                OpenExpression $ Set.singleton x) $ EvalM . right <$> mval

-- |Updates the value of a given variable.
setVar :: Var -> Value -> EvalM ()
setVar x v = do
  s <- get
  let env = evalEnv s
  put $ s { evalEnv = env { varMap = Map.insert x v $ varMap env } }

-- |Updates the "most recent variable" of the current evaluation.
setMostRecent :: Var -> EvalM ()
setMostRecent x = do
  s <- get
  let env = evalEnv s
  put $ s { evalEnv = env { lastVar = Just x } }

-- |Retrieves the entire variable environment.
getEnv :: EvalM EvalEnv
getEnv = evalEnv <$> get

-- |Raises an evaluation error.
raiseEvalError :: EvalError -> EvalM a
raiseEvalError = EvalM . left

-- |Retrieves the clauses in a given small-step evaluation.
getClauses :: EvalM [Clause]
getClauses = evalClauses <$> get

-- |Sets the clauses in a given small-step evaluation.
setClauses :: [Clause] -> EvalM ()
setClauses cls =
  modify $ \s -> EvalState (evalEnv s) cls (evalVarIndex s)

-- |Replaces the first clause of the clause list with the provided clauses.
replaceFirstClause :: [Clause] -> EvalM ()
replaceFirstClause cls =
  modify $ \s -> EvalState (evalEnv s) (cls ++ drop 1 (evalClauses s))
                           (evalVarIndex s)
                           
-- |Fetches the next fresh variable index and increments the fresh variable
--  counter.
freshVarIndex :: EvalM Integer
freshVarIndex = do
  s <- get
  put $ EvalState (evalEnv s) (evalClauses s) (evalVarIndex s + 1)
  return $ evalVarIndex s
  
-- |Obtains fresh variables.  This is a general form of the fresh variable
--  functions below.
freshVar :: Var -> EvalM Var
freshVar xx = do
  idx <- freshVarIndex
  return $ uncurry GenVar (destruct xx) idx
  where
    destruct :: Var -> (Origin, String)
    destruct x = case x of
                  Var o s -> (o,s)
                  GenVar o s _ -> (o,s)
