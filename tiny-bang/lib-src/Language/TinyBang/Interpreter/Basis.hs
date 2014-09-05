{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Language.TinyBang.Interpreter.Basis
( EvalEnv(..)
, EvalState(..)
, EvalError(..)
, EvalM(..)
, runEvalM
, varLookup
, setVar
, setMostRecent
, getEnv
, returnTBChar
, raiseEvalError
, getClauses
, setClauses
, replaceFirstClause
, freshVar
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display

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
  | BuiltinBadOperandCount Origin BuiltinOp Int Int
      -- ^Generated when a builtin operation has the wrong number of arguments.
      --  Includes the expected number of arguments and the number which
      --  appeared.
  | BuiltinBadOperandType Origin BuiltinOp Int Var
      -- ^Generated when a builtin operation has the wrong type of argument.
      --  Includes the argument index and the variable which appeared there.
  deriving (Eq, Ord, Show)

instance Display EvalError where
  makeDoc err = text $ show err

-- |The monad in which small-step evaluation takes place.
newtype EvalM a
  = EvalM { unEvalM :: EitherT EvalError (StateT EvalState IO) a }
  deriving (Functor, Applicative, Monad, MonadState EvalState, MonadIO)

-- |Executes an evaluation.
runEvalM :: EvalState -> EvalM a -> IO (Either EvalError a, EvalState)
runEvalM s x =
  let unwrapped = unEvalM x in -- EitherT EvalError (StateT EvalState Identity) a
  let unEithered = runEitherT unwrapped in -- StateT EvalState Identity (Either EvalError a)
  let unStated = runStateT unEithered s in -- Identity (Either EvalError a, EvalState)
  unStated
  --runIdentity $ runStateT (runEitherT (unEvalM x)) s  -- Identity (EvalState, Either EvalError a)

returnTBChar :: Origin -> EvalM Value
returnTBChar o =
  getTBChar >>= processTBChar o
  where
  processTBChar :: Origin -> Char -> EvalM Value
  processTBChar o' c =
    return $ VPrimitive o' (VChar o' c)
  getTBChar :: EvalM Char
  getTBChar = do
    c <- liftIO getChar
    return c  

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
freshVar (Var o n _) = Var o n . Just <$> freshVarIndex
