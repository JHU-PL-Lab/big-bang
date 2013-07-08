{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

{-|
  This module provides a mechanism for detecting inconsistency in closed
  constraint databases.
-}
module Language.PatBang.TypeSystem.Inconsistency
( Inconsistency(..)
, determineInconsistencies
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Language.PatBang.Ast
import Language.PatBang.Display
import Language.PatBang.Logging
import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Fibrations
import Language.PatBang.TypeSystem.Monad.Trans.CReader
import Language.PatBang.TypeSystem.Monad.Trans.Flow
import Language.PatBang.TypeSystem.Relations
import Language.PatBang.TypeSystem.Types

$(loggingFunctions)

data Inconsistency db
  = ApplicationFailure
      ApplicationConstraint -- ^ The triggering constraint
      (ProjectionResult db) -- ^ The projection of scapes
      (ApplicationCompatibilityResult db)
        -- ^ The result of application compatibility
  | IntegerOperationFailure
      OperationConstraint -- ^ The triggering constraint
      (ProjectionResult db) -- ^ The projection result for the left side
      (ProjectionResult db) -- ^ The projection result for the right side
  deriving (Eq, Ord, Show)

instance (ConstraintDatabase db, Display db) => Display (Inconsistency db) where
  makeDoc incon = case incon of
    ApplicationFailure ac mpr acr ->
      text "ApplicationFailure" <+> parens (makeDoc ac)
        <+> parens (makeDoc mpr) <+> parens (makeDoc acr)
    IntegerOperationFailure oc spr1 spr2 ->
      text "ApplicationFailure" <+> parens (makeDoc oc)
        <+> parens (makeDoc spr1) <+> parens (makeDoc spr2)

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
  (scapeVars, pFib) <- liftProjToIncon $ project projScape a1
  let scapes = map (uncurry Scape) scapeVars
  (Nothing, cFib) <- liftCompatToIncon $ checkApplicationCompatible a2 scapes
  let projRes = ProjectionResult projFun a1 $
                  ProjectionResultFunForm scapes $ pFib $
                    zip unexpandeds unexpandeds 
  let appCRes = ApplicationCompatibilityResult a2 scapes Nothing cFib
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
  (x1, r1) <- liftProjToIncon $ projectSinglePrimResult projInt a1
  (x2, r2) <- liftProjToIncon $ projectSinglePrimResult projInt a2
  guard $ not x1 || not x2
  return $ IntegerOperationFailure oc r1 r2

-- |Lifts a projection operation into the inconsistency monad.
liftProjToIncon :: (Functor m, Monad m) => ProjM db m a -> InconM db m a
liftProjToIncon = id

-- |Lifts a compatibility operation into the inconsistency monad.
liftCompatToIncon :: (Functor m, Monad m) => CompatM db m a -> InconM db m a
liftCompatToIncon = id