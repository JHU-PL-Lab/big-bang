{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.TinyBangNested.ATranslator.Monad
( ATranslationM(..)
, ATranslationState(..)
, runATranslationM
, freshVar
--, transVar
, bindVar
, useVar
, bracketScope
) where

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast as TBA
import Language.TinyBangNested.Ast as TBN

newtype ATranslationM a
  = ATranslationM
      { unATranslationM :: State ATranslationState a
      }
  deriving (Monad, MonadState ATranslationState, Functor, Applicative)

data ATranslationState
  = ATranslationState
      { freshVarIndex :: Int
      , varMaps :: [Map TBN.Var TBA.Var]
      , usedVars :: Set TBA.Var
      }

-- |Executes an A-translation with a fixed initial state.
runATranslationM :: ATranslationM a -> a
runATranslationM x =
  let initialState =
        ATranslationState
          { freshVarIndex = 0
          , varMaps = [Map.empty]
          , usedVars = Set.empty
          }
  in
  evalState (unATranslationM x) initialState
  
-- |Creates a new A-translation variable based on a prefix string.
newVar :: String -> ATranslationM TBA.Var
newVar ident = do
  -- TODO: modify this routine so it can capture some of the TBN origin data in
  --       the resulting TBA AST
  s <- get
  let i = freshVarIndex s
  let x = TBA.Var generated (TBA.IdentifierVar $ ident ++ "_" ++ show i) Nothing
  if x `Set.member` usedVars s
    then do
      put $ s { freshVarIndex = i + 1 }
      newVar ident
    else do
      put $ s { freshVarIndex = i + 1
              , usedVars = Set.insert x $ usedVars s
              }
      return x

-- |Obtains an A-translation fresh variable.
freshVar :: ATranslationM TBA.Var
freshVar = newVar "a"

-- |Retrieves a mapping of all bound variables.
curVarMap :: ATranslationM (Map TBN.Var TBA.Var)
curVarMap = Map.unions <$> varMaps <$> get 

-- TODO: separate between transVar for definitions and for uses; this way,
--       we refuse to A-translate open expressions
-- |Obtains an A-translation variable mapped from the provided TBN variable.
--  This mapping is stable; the same result is returned for a given TBN variable
--  throughout the course of an A-translation.
{-
transVar :: TBN.Var -> ATranslationM TBA.Var
transVar x = do
  mv <- Map.lookup x <$> curVarMap
  case mv of
    Just x' -> return x'
    Nothing -> do
      let (TBN.Var _ ident) = x
      x' <- newVar ident
      maps <- varMaps <$> get
      maps' <- case maps of
                [] -> error "Empty variable stack!" -- TODO: monadic failure
                m:maps' -> return $ Map.insert x x' m : maps'
      modify $ \s' -> s' { varMaps = maps' }
      return x'
-}

-- |Looks up a variable in the variable map, assuming it is defined.
useVar :: TBN.Var -> ATranslationM TBA.Var
useVar x = do
  mv <- Map.lookup x <$> curVarMap
  case mv of
    Just x' -> return x'
    Nothing -> error "Undefined variable!" -- TODO: monadic failure

bindVar :: TBN.Var -> ATranslationM TBA.Var
bindVar x = do
  let (TBN.Var _ ident) = x
  x' <- newVar ident
  maps <- varMaps <$> get
  maps' <- case maps of
            [] -> error "Empty variable stack!" -- TODO: monadic failure
            m:maps' -> return $ Map.insert x x' m : maps'
  modify $ \s' -> s' { varMaps = maps' }
  return x'
      
-- |Performs the provided A-translation in a new scope.
bracketScope :: ATranslationM x -> ATranslationM x
bracketScope computation = do
  startScope
  result <- computation
  stopScope
  return result

-- |Begins a new scope in the A-translation.  This is used to ensure that
--  variable scoping and shadowing work correctly.
startScope :: ATranslationM ()
startScope = modify $ \s -> s { varMaps = Map.empty : varMaps s }

-- |Ends the current scope in the A-translation.
stopScope :: ATranslationM ()
stopScope = do
  maps <- varMaps <$> get
  case maps of
    [] -> error "Empty variable stack!" -- TODO: monadic failure
    _:[] -> error "Tried to pop top-level frame!" -- TODO: monadic failure
    _:maps' -> modify $ \s -> s { varMaps = maps' }
