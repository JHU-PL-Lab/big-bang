{-# LANGUAGE ScopedTypeVariables #-}

module Language.PatBang.Interpreter.Basis
( EvalEnv(..)
, EvalState(..)
, UsedVars(..)
, EvalError(..)
, EvalM
, evalError
, flowLookup
, setClauses
, replaceFirstClause
, modifyUsedVars
, freshFlowVar
) where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast
import Language.PatBang.Display

-- |A data structure containing an evaluation environment.
data EvalEnv = EvalEnv
                { flowVarMap :: Map FlowVar Value
                  -- ^ A map indicating paths for flow
                , lastVar :: Maybe FlowVar
                  -- ^ The most recent evaluated flow variable (or Nothing if
                  --   the most recent evaluation did not define a flow
                  --   variable)
                }
  deriving (Eq, Ord, Show)
                
-- |A data strcture representing evaluation state.
data EvalState = EvalState
                { evalEnv :: EvalEnv
                  -- ^ The current evaluation environment
                , evalClauses :: [Clause]
                  -- ^ The current evaluation clauses
                , evalUsedVars :: UsedVars
                  -- ^ The set of variables used so far
                }
  deriving (Eq, Ord, Show)

-- |A set containing all used variables.
data UsedVars = UsedVars (Set String) Integer
  deriving (Eq, Ord, Show)
                
-- |A data structure representing evaluation errors.
data EvalError
  = IllFormedExpression IllFormedness
  | OpenExpression (Set FlowVar)
  | FlowVarNotClosed FlowVar
  | MismatchedPatternSubstitution [PatVar] [PatternBody]
  | ProjectionFailure FlowVar AnyProjector
  | ApplicationFailure FlowVar FlowVar
  deriving (Eq, Ord, Show)
  
instance Display EvalError where
  makeDoc err = text $ show err

-- |The monad in which small-step evaluation takes place.
type EvalM a = EitherT EvalError (State EvalState) a

-- |Reports an evaluation error.
evalError :: EvalError -> EvalM a
evalError = left

-- |Performs a value lookup on a given flow variable.
flowLookup :: FlowVar -> EvalM Value
flowLookup x = do
  env <- evalEnv <$> get
  let mval = Map.lookup x $ flowVarMap env
  fromMaybe (left $ FlowVarNotClosed x) $ right <$> mval

-- |Sets the clauses in a given small-step evaluation.
setClauses :: [Clause] -> EvalM ()
setClauses cls =
  modify $ \s -> EvalState (evalEnv s) cls (evalUsedVars s)

-- |Replaces the first clause of the clause list with the provided clauses.
replaceFirstClause :: [Clause] -> EvalM ()
replaceFirstClause cls =
  modify $ \s -> EvalState (evalEnv s) (cls ++ drop 1 (evalClauses s))
                           (evalUsedVars s)
                           
-- |Modifies the @UsedVars@ state component.
modifyUsedVars :: (UsedVars -> UsedVars) -> EvalM ()
modifyUsedVars f =
  modify $ \s -> EvalState (evalEnv s) (evalClauses s) (f $ evalUsedVars s)
  
-- |Obtains fresh variables.  This is a general form of the fresh variable
--  functions below.
freshVar :: forall a b.
            (a -> (Origin, String)) -> (Origin -> String -> Integer -> b)
         -> (b -> String)
         -> a -> EvalM b
freshVar destruct construct toString xx =
  let (origin,base) = destruct xx in findFresh origin base
  where
    findFresh :: Origin -> String -> EvalM b
    findFresh o b = do
      UsedVars u' i' <- evalUsedVars <$> get
      let xx' = construct o b i'
      if toString xx' `Set.member` u'
        then do
          modifyUsedVars (\(UsedVars u i) -> UsedVars u $ i + 1)
          findFresh o b
        else do
          modifyUsedVars (\(UsedVars u i) ->
            UsedVars (Set.insert (toString xx') u) i)
          return xx'

-- |Obtains a fresh flow variable
freshFlowVar :: FlowVar -> EvalM FlowVar
freshFlowVar = freshVar destructFlowVarToPair GenFlowVar unFlowVar

destructFlowVarToPair :: FlowVar -> (Origin, String)
destructFlowVarToPair x' = case x' of
      FlowVar o s -> (o,s)
      GenFlowVar o s _ -> (o,s)
