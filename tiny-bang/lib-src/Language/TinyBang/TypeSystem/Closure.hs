{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances, GeneralizedNewtypeDeriving  #-}
{-|
  This module implements the TinyBang constraint closure relation.
-}
module Language.TinyBang.TypeSystem.Closure
( calculateClosure
) where

import Control.Applicative
import Control.Monad

import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Matching
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.NonDet
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

{- TODO: improve this implementation.  At the moment, a lot of cases are being
         examined multiple times.  One possibility is a two-level implementation
         in which the inner level operates under a "sloppy" cache, ignoring
         e.g. call sites which have already been visited.  The outer level can
         then clear the cache at each step and proceed until no progress is
         made even with an empty cache. 
-}

-- |Calculates the transitive closure of the provided constraint set under the
--  TinyBang constraint closure rules.
calculateClosure :: (ConstraintDatabase db, Display db)
                 => db -> db
calculateClosure db =
  let db' = calculateClosureStep db in
  if db == db' then db' else calculateClosure db'

-- |Calculates a single step of constraint closure.
calculateClosureStep :: forall db. (ConstraintDatabase db, Display db)
                     => db -> db
calculateClosureStep db =
  bracketLog _debugI
    (display $ text "Calculating closure step for:" <+> makeDoc db)
    (\result -> display $ text "Finished closure step:" <+> makeDoc result) $
    foldr CDb.union db $ concatMap (`runClosureStepM` db) rules
  where
    rules :: [ClosureStepM db db]
    rules =
      [ closeTransitivity
      , closeApplication
      ]

newtype ClosureStepM db a
  = ClosureStepM
      { unClosureStepM :: NonDetT (CReader db) a
      }
  deriving ( Monad, Functor, Applicative, MonadCReader db, MonadNonDet
           , MonadPlus)

runClosureStepM :: (ConstraintDatabase db, Display db)
                => ClosureStepM db a -> db -> [a]
runClosureStepM x db = runCReader (runNonDetT (unClosureStepM x)) db

closeTransitivity :: (ConstraintDatabase db, Display db) => ClosureStepM db db
closeTransitivity = bracketLogM _debugI
  "Calculating transitivity closure"
  (\res -> display $ text "Transitivity results:" <+> makeDoc res) $
  do
    (t,a1) <- join $ choose <$> queryDb QueryAllTypesLowerBoundingTVars
    a2 <- join $ choose <$> queryDb (QueryLowerBoundingTVarsOfTVar a1)
    let h = DerivedFromClosure $ TransitivityRule t a1 a2
    return $ CDb.singleton $ t <: a2 .: h

closeApplication :: forall db. (ConstraintDatabase db, Display db)
                 => ClosureStepM db db
closeApplication = bracketLogM _debugI
  "Calculating application closure"
  (\res -> display $ text "Application results:" <+> makeDoc res) $
  do
    (a0,a1,a2) <- join $ choose <$> queryDb QueryAllApplications
    db <- askDb
    (argtov, mbinds) <- choose $ matches a2 a0 a1 db
    case mbinds of
      Nothing ->
        let h = DerivedFromClosure $ ApplicationRule a0 a1 a2 Nothing argtov in
        return $ CDb.singleton $ InconsistencyConstraint h $
          ApplicationFailure a0 a1 a2 argtov
      Just (scapet, (a',db')) ->
        let h = DerivedFromClosure $
                  ApplicationRule a0 a1 a2 (Just scapet) argtov in
        let db'' = CDb.add (a' <: a2 .: h) db' in
        let cntr = mkCntr a2 in
        return $ CDb.polyinstantiate cntr db''
  where
    mkCntr a@(TVar x pcntr) = case unPossibleContour pcntr of
      Nothing -> error $
                  "Attempted to polyinstantiate call site with no contour! (" ++
                  display a ++ ")"
      Just cntr ->
        extend x cntr
