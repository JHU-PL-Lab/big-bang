{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Language.TinyBang.TypeSystem.Simple.Closure
( computeClosure
) where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.NonDet
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Simple.Data
import Language.TinyBang.TypeSystem.Simple.Matching
import Language.TinyBang.TypeSystem.Simple.Polymorphism
import Language.TinyBang.TypeSystem.Simple.Variables

computeClosure :: ConstraintSet -> (Set Inconsistency, ConstraintSet)
computeClosure cs =
  closeToFixedPoint (mempty, cs) closureStep
  where
    closeToFixedPoint :: (Eq a) => a -> (a -> a) -> a
    closeToFixedPoint x f =
      let out = f x in
      if x == out then out else closeToFixedPoint out f
    closureStep :: (Set Inconsistency, ConstraintSet)
                -> (Set Inconsistency, ConstraintSet)
    closureStep (incons, cs) =
      let (incons', cs') = singleClosurePass cs in
      (incons `mappend` incons', cs `mappend` cs')
  
singleClosurePass :: ConstraintSet -> (Set Inconsistency, ConstraintSet)
singleClosurePass cs =
  mconcat $ map runClosureRule [transitivityRule, applicationRule]
  where
    runClosureRule :: ClosureM ConstraintSet
                   -> (Set Inconsistency, ConstraintSet)
    runClosureRule rule = second mconcat $ runClosureM rule cs
  
newtype ClosureM a
  = ClosureM
      { unClosureM :: EitherT (Set Inconsistency)
                        (NonDetT (Reader ConstraintSet)) a
      }
  deriving ( Monad, MonadPlus, Applicative, Functor, MonadReader ConstraintSet
           , MonadNonDet)

runClosureM :: ClosureM a -> ConstraintSet -> (Set Inconsistency, [a])
runClosureM x cs =
  let results = runReader (runNonDetT $ runEitherT $ unClosureM x) cs in
  let pairedResults = map (either (,[]) ((Set.empty,) . (:[]))) results in
  mconcat pairedResults

reportInconsistency :: Inconsistency -> ClosureM a
reportInconsistency = ClosureM . left . Set.singleton

transitivityRule :: ClosureM ConstraintSet
transitivityRule = do
  cs <- Set.toList <$> unConstraintSet <$> ask
  LowerBoundConstraint ft a1 <- choose cs
  IntermediateConstraint a1' a2 <- choose cs
  guard $ a1 == a1'
  return $ ConstraintSet $ Set.singleton $ ft <: a2

applicationRule :: ClosureM ConstraintSet
applicationRule = do
  cs <- Set.toList <$> unConstraintSet <$> ask
  ApplicationConstraint a0 a1 a2 <- choose cs
  matches <- findApplicationMatches a0 a1 <$> ask
  match <- choose matches
  case match of
    SuccessfulMatchResult
      { bodyRoot = a1'
      , bodyConstraints = cs1'
      , bindings = cs1''
      } ->
      let sigma = polyinstFnForConstraintSet (cs1' `mappend` cs1'') a2 in
      let (a2',cs2') = substituteVars sigma (a1',cs1') in
      let cs2'' = tFresh sigma cs1'' in
      return $ mconcat [cs2', cs2'', ConstraintSet $ Set.singleton $ a2' <: a2]
    FailedMatchResult ->
      -- TODO: put info in the inconsistency type
      reportInconsistency Inconsistency
