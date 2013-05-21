{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

{-|
  This module provides a mechanism for detecting inconsistency in closed
  constraint databases.
-}
module Language.TinyBang.TypeSystem.Inconsistency
( Inconsistency(..)
, determineInconsistencies
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Maybe

import Language.TinyBang.Ast
import Language.TinyBang.Display
import Language.TinyBang.Logging
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.Flow
import Language.TinyBang.TypeSystem.Relations

$(loggingFunctions)

data Inconsistency db
  = ApplicationFailure
      ApplicationConstraint -- ^ The triggering constraint
      (MultiProjectionResult db) -- ^ The projection of scapes
      (ApplicationCompatibilityResult db)
        -- ^ The result of application compatibility
  | IntegerOperationFailure
      OperationConstraint -- ^ The triggering constraint
      (SingleProjectionResult db) -- ^ The projection result for the left side
      (SingleProjectionResult db) -- ^ The projection result for the right side
  deriving (Eq, Ord, Show)

determineInconsistencies :: forall db.
                            (ConstraintDatabase db, Display db)
                         => db -> Either (ProjectionError db) [Inconsistency db]
determineInconsistencies db = do -- Either (ProjectionError db)
  iss <- mapM (`expandInconM` db) inconsistencies
  return $ concat iss
  where
    inconsistencies :: ( ConstraintDatabase db, Monad m, Functor m
                       , Applicative m, Display db, MonadCReader db m )
                    => [InconM db m (Inconsistency db)]
    inconsistencies = [ findApplicationInconsistencies
                      , findIntegerOperationInconsistencies ]

type InconM db m = FlowT (EitherT (ProjectionError db) m)

-- |A routine for expanding an @InconM@ monadic value.
expandInconM :: (ConstraintDatabase db)
             => InconM db (CReader db) a -> db
             -> Either (ProjectionError db) [a]
expandInconM calc = runCReader (runEitherT $ runFlowT calc)

findApplicationInconsistencies :: ( ConstraintDatabase db, Monad m, Functor m
                                  , Applicative m, Display db
                                  , MonadCReader db m )
                               => InconM db m (Inconsistency db)
findApplicationInconsistencies = do
  appc@(ApplicationConstraint a1 a2 _) <-
      flow $ lift $ getApplicationConstraints <$> askDb
  (scapes, pFib) <- liftProjToIncon $ project projFun a1
  let arg = ArgVal a2
  (Nothing, cFib) <- liftCompatToIncon $ checkApplicationCompatible arg scapes
  let projRes = MultiProjectionResult projFun a1 scapes pFib
  let appCRes = ApplicationCompatibilityResult arg scapes Nothing cFib
  return $ ApplicationFailure appc projRes appCRes

findIntegerOperationInconsistencies :: ( ConstraintDatabase db, Monad m
                                       , Functor m, Applicative m, Display db
                                       , MonadCReader db m )
                                    => InconM db m (Inconsistency db)
findIntegerOperationInconsistencies = do
  oc@(OperationConstraint a1 _ a2 _) <-
      flow $ lift $ (++)
                      <$> (getIntegerOperationConstraints <$> askDb)
                      <*> (getIntegerCalculationConstraints <$> askDb)
  (mt1, r1) <- liftProjToIncon $ projectSingleResult projInt a1
  (mt2, r2) <- liftProjToIncon $ projectSingleResult projInt a2
  guard $ isNothing mt1 || isNothing mt2
  return $ IntegerOperationFailure oc r1 r2

-- |Lifts a projection operation into the inconsistency monad.
liftProjToIncon :: (Functor m, Monad m) => ProjM db m a -> InconM db m a
liftProjToIncon = id

-- |Lifts a compatibility operation into the inconsistency monad.
liftCompatToIncon :: (Functor m, Monad m) => CompatM db m a -> InconM db m a
liftCompatToIncon = id
