{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.TinyBangNested.ATranslator.Monad
( ATranslationM(..)
, ATranslationState(..)
, runATranslationM

, freshVar
, transVar
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
      , varMap :: Map TBN.Var TBA.Var
      , usedVars :: Set TBA.Var
      }

-- |Executes an A-translation with a fixed initial state.
runATranslationM :: ATranslationM a -> a
runATranslationM x =
  let initialState =
        ATranslationState
          { freshVarIndex = 0
          , varMap = Map.empty
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
  let x = TBA.Var generated (TBA.IdentifierVar $ "a_" ++ show i) Nothing
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
freshVar = newVar "a_"

-- |Obtains an A-translation variable mapped from the provided TBN variable.
--  This mapping is stable; the same result is returned for a given TBN variable
--  throughout the course of an A-translation.
transVar :: TBN.Var -> ATranslationM TBA.Var
transVar x = do
  s <- get
  let mv = Map.lookup x $ varMap s
  case mv of
    Just x' -> return x'
    Nothing -> do
      let (TBN.Var _ ident) = x
      x' <- newVar ident
      modify $ \s' -> s' { varMap = Map.insert x x' $ varMap s'  }
      return x'
