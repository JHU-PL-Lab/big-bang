{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.TinyBang.TypeSystem.Simple.Matching
( findApplicationMatches
, MatchResult(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.NonDet
import Data.Monoid
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Simple.Compatibility
import Language.TinyBang.TypeSystem.Simple.Data

-- |A computable function modeling the behavior of application matching.
findApplicationMatches :: TVar -> TVar -> ConstraintSet -> [MatchResult]
findApplicationMatches a0 a1 cs =
  map fst $ runMatchingM (matching a0 a1 $ PatternTypeSet mempty) cs 

-- |A data type containing the pieces of a match result.
data MatchResult
  = SuccessfulMatchResult
      { bodyRoot :: TVar
      , bodyConstraints :: ConstraintSet
      , bindings :: ConstraintSet
      }
  | FailedMatchResult

newtype MatchingM a
  = MatchingM { unMatchingM :: NonDetT (Reader ConstraintSet) a }
  deriving ( Monad, MonadPlus, Applicative, Functor, MonadNonDet
           , MonadReader ConstraintSet)

runMatchingM :: MatchingM a -> ConstraintSet -> [a]
runMatchingM x = runReader (runNonDetT $ unMatchingM x)

-- |Performs an application matching computation.  The result is a pair between
--  the result obtained and the patterns which were matched in the process.
matching :: TVar -> TVar -> PatternTypeSet
         -> MatchingM (MatchResult, PatternTypeSet)
matching a0 a1 patsN = do
  -- First, perform steps common to all rules.  We start by picking a lower
  -- bound for the function variable.
  globalConstraints <- ask
  LowerBoundConstraint rt@(FilteredType t _ _) a0' <- choose $
    Set.toList $ unConstraintSet globalConstraints
  guard $ a0 == a0'
  -- At this point, we can perform the sensibility check.
  guard $ sensible rt globalConstraints
  -- Next, decide what to do based on the form of the concrete lower bound.
  case t of
    TScape pat a body -> do
      -- Either Function Match or Function Mismatch must apply.
      compatResult <- choose $ Set.toList $
                        findCompatibilityCases a1 globalConstraints pat patsN
      let matchResult = case compatResult of
                          Nothing ->
                            FailedMatchResult 
                          Just bindingsCs ->
                            SuccessfulMatchResult
                              { bodyRoot = a
                              , bodyConstraints = body
                              , bindings = bindingsCs
                              }
      return (matchResult, PatternTypeSet $ Set.singleton pat)
    TOnion a2 a3 -> do
      -- Either Onion Left or Onion Right must apply.
      (compatResult, patsN') <- matching a2 a1 patsN
      case compatResult of
        SuccessfulMatchResult{} ->
          return (compatResult, patsN') 
        FailedMatchResult ->
          -- The Onion Right rule applies: we failed to match on the left side,
          -- so carry into the right (including the failures that we must prove
          -- on the argument).
          matching a3 a1 $ patsN `mappend` patsN'
    _ ->
      -- Non-Function must apply.
      return (FailedMatchResult, PatternTypeSet mempty)
