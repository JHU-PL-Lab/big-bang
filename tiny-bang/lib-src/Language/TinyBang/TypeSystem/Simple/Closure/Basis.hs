{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Language.TinyBang.TypeSystem.Simple.Closure.Basis
( ClosureM(..)
, runClosureM
, reportTypecheckError
, mergeTypecheckError
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.NonDet
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Simple.Data

newtype ClosureM a
  = ClosureM
      { unClosureM :: EitherT (Set TypecheckError)
                        (NonDetT (Reader ConstraintSet)) a
      }
  deriving ( Monad, MonadPlus, Applicative, Functor, MonadReader ConstraintSet
           , MonadNonDet)

runClosureM :: ClosureM a -> ConstraintSet -> (Set TypecheckError, [a])
runClosureM x cs =
  let results = runReader (runNonDetT $ runEitherT $ unClosureM x) cs in
  let pairedResults = map (either (,[]) ((Set.empty,) . (:[]))) results in
  mconcat pairedResults

reportTypecheckError :: TypecheckError -> ClosureM a
reportTypecheckError = ClosureM . left . Set.singleton

mergeTypecheckError :: ClosureM (Either TypecheckError a) -> ClosureM a
mergeTypecheckError x = do
  x' <- x
  case x' of
    Left err -> reportTypecheckError err
    Right x'' -> return x''
