{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, TemplateHaskell #-}
module Language.TinyBang.TypeSystem.Simple.Closure.Basis
( ClosureM(..)
, runClosureM
, reportInconsistency
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

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Simple.Data
import Language.TinyBang.TypeSystem.Simple.Matching
import Language.TinyBang.TypeSystem.Simple.Polymorphism
import Language.TinyBang.TypeSystem.Simple.Variables
import Language.TinyBang.Utils.Display as D
import Language.TinyBang.Utils.Logger

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

