{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-|
  This module implements the PatBang constraint closure relation.
-}
module Language.PatBang.TypeSystem.Closure
( ClosureError(..)
, calculateClosure
) where

import           Control.Applicative
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either
import qualified Data.Set                   as Set

import Language.PatBang.Ast
import Language.PatBang.Display
import Language.PatBang.Logging
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.Contours
import Language.PatBang.TypeSystem.Fibrations
import Language.PatBang.TypeSystem.Monad.Trans.CReader
import Language.PatBang.TypeSystem.Monad.Trans.Flow
import Language.PatBang.TypeSystem.Relations
import Language.PatBang.TypeSystem.Types

$(loggingFunctions)

-- |A data structure representing errors in constraint closure.
data ClosureError db
  = ClosureCompatibilityFailure (CompatibilityError db)
  | ClosureProjectionFailure (ProjectionError db)
  deriving (Eq, Ord, Show)
instance (ConstraintDatabase db, Display db) => Display (ClosureError db) where
  makeDoc err = case err of
    ClosureCompatibilityFailure err' -> makeDoc err'
    ClosureProjectionFailure err' -> makeDoc err'

-- |Calculates the transitive closure of the provided constraint database.  The
--  transitive closure of a PatBang database is only confluent up to
--  equivalence of contour folding; this function will produce a representative
--  of the appropriate equivalence class.
calculateClosure :: (ConstraintDatabase db, Display db, Ord db)
                 => db -> Either (ClosureError db) db
calculateClosure db =
  case calculateClosureStep db of
    Left err -> Left err
    Right db' ->
      if db == db' then Right db else calculateClosure db'

-- |The monad under which each closure step occurs.
type ClosureM db m = FlowT (EitherT (ClosureError db) m)

-- |A routine for expanding a @ClosureM@ monadic value.
expandClosureM :: (ConstraintDatabase db)
               => ClosureM db (CReader db) a -> db
               -> Either (ClosureError db) [a]
expandClosureM calc = runCReader (runEitherT $ runFlowT calc)

-- |Calculates a single step of closure.
calculateClosureStep :: forall db. (ConstraintDatabase db, Display db, Ord db)
                     => db -> Either (ClosureError db) db
calculateClosureStep db =
  do -- Either (ClosureError db)
    dbss <- mapM (`expandClosureM` db) monotonicClosures
    _debug $ "Monotonic closure rules gave: " ++ display (debugFiltering dbss)
    let dbPlusMonotone = foldr (flip union) db $ concat dbss 
    answer <- if dbPlusMonotone /= db
                then return dbPlusMonotone
                else calculateApplicationClosure db
    _debug $ "Closure step yields: " ++ display answer
    _debug $ "Contours in current database: " ++ display (getAllContours answer)
    return answer
  where
    monotonicClosures :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                         , Applicative m)
                      => [ClosureM db m db]
    monotonicClosures = [ closeTransitivity
                        , closeIntegerCalculations
                        , closeIntegerComparisons
                        , closeEquality
                        ]
    debugFiltering :: [[db]] -> [Constraint db]
    debugFiltering dbss = concat $ concat $ (`map` dbss) $ map $
      Set.toList . (`Set.difference` getAllConstraints db) . getAllConstraints

-- * Application closure

-- |Calculates application closure on the given database.  This routine will
--  perform some number of application closures but may not perform all of them.
--  Nonetheless, this function guarantees that progress will be made if any
--  progress is possible; if the input and output are equal, the exist no
--  additional application closures to perform.
calculateApplicationClosure :: forall db. (ConstraintDatabase db, Display db)
                            => db -> Either (ClosureError db) db
calculateApplicationClosure db =
  do -- Either (ClosureError db)
    effects <- mapM (`expandClosureM` db) applicationClosures
    return $ applyEffects $ concat effects
  where
    applicationClosures :: ( Display db, ConstraintDatabase db
                           , MonadCReader db m , Functor m, Applicative m)
                        => [ClosureM db m (db, Maybe Contour)]
    applicationClosures = [ normalApplicationClosures ]
    applyEffects effects = case effects of
      [] -> db
      (db', mcn):effects' ->
        let f = case mcn of
                  Nothing -> id
                  Just cn -> replaceContours cn
        in
        let db'' = f $ db `union` db' in
        if db'' /= db
          then db''
          else applyEffects effects'

-- |Determines the effects of normal application closure in a given database.
--  This operation produces pairs of constraint sets and contours to apply to
--  the database, but it does not perform any replacement.  If the contour is a
--  @Nothing@, no replacement is to occur.  The resulting list of closures is
--  produced in arbitrary order.
normalApplicationClosures :: forall db m.
                             ( Display db, ConstraintDatabase db
                             , MonadCReader db m , Functor m, Applicative m)
                          => ClosureM db m (db, Maybe Contour)
normalApplicationClosures = do
  appc@(ApplicationConstraint a1 a2 a3) <-
      flow $ lift $ getApplicationConstraints <$> askDb
  (scapeVars, pFib, ffibs) <-
      liftProjToClosure $ project projScape a1 Unexpanded
  let scapes = map (uncurry Scape) scapeVars
  (mdata@(Just (a4, db')), cFib) <- liftCompatToClosure $
                  checkApplicationCompatible a2 scapes
  cn <- cNew a3 <$> askDb
  let wiringConstraint = cwrap $ IntermediateConstraint a4 a3
      wiringHistory = DerivedFromClosure $ ApplicationRule appc
                        (ProjectionResult projScape a1 $
                          ProjectionResultScapeForm scapes $ pFib ffibs)
                        (ApplicationCompatibilityResult a2 scapes mdata cFib)
                        cn
  let db'' = instantiateContours Set.empty cn $
                add wiringConstraint wiringHistory db'
  return (db'', Just cn)

-- * Closure routines

-- |Calculates transitivity closures in the constraint database.  The resulting
--  databases contain the new constraints concluded from this operation.
closeTransitivity :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                     , Applicative m)
                  => ClosureM db m db
closeTransitivity = do
  tc@(TypeConstraint t a) <- flow $ lift $ getTypeConstraints <$> askDb
  ic@(IntermediateConstraint _ a') <-
      flow $ lift $ getIntermediateConstraintsByLowerBound a <$> askDb
  let history = DerivedFromClosure $ TransitivityRule tc ic
  return $ singleton (cwrap $ TypeConstraint t a') history

-- |Calculates integer operation closures in the constraint database.
closeIntegerCalculations :: ( ConstraintDatabase db, MonadCReader db m
                            , Functor m, Applicative m, Display db )
                         => ClosureM db m db
closeIntegerCalculations = do
  oc@(OperationConstraint a2 _ a3 a1) <-
      flow $ lift $ getIntegerCalculationConstraints <$> askDb
  (True,r1) <- liftProjToClosure $ projectSinglePrimResult projInt a2 Unexpanded
  (True,r2) <- liftProjToClosure $ projectSinglePrimResult projInt a3 Unexpanded
  let history = DerivedFromClosure $ IntegerOperationRule oc r1 r2
  return $ singleton
    (WrapTypeConstraint $ TypeConstraint (Primitive primInt) a1) history

-- |Calculates integer operation comparisons in the constraint database.  This
--  does not include equality, which is more general.
closeIntegerComparisons :: ( ConstraintDatabase db, MonadCReader db m
                           , Functor m, Applicative m, Display db )
                        => ClosureM db m db
closeIntegerComparisons = do
  oc@(OperationConstraint a2 _ a3 a1) <-
      flow $ lift $ getIntegerOperationConstraints <$> askDb
  (True,r1) <- liftProjToClosure $ projectSinglePrimResult projInt a2 Unexpanded
  (True,r2) <- liftProjToClosure $ projectSinglePrimResult projInt a3 Unexpanded
  doEqualityFor a1 oc $ DerivedFromClosure $ IntegerCalculationRule oc r1 r2

-- |Calculates equality operations in the constraint database.
closeEquality :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                 , Applicative m )
              => ClosureM db m db
closeEquality = do
  oc@(OperationConstraint _ _ _ a1) <-
      flow $ lift $ getEqualityConstraints <$> askDb
  doEqualityFor a1 oc $ DerivedFromClosure $ EqualityRule oc

-- * Utility functions

-- |Lifts a projection operation into the closure monad.
liftProjToClosure :: (Functor m, Monad m) => ProjM db m a -> ClosureM db m a
liftProjToClosure = flow . bimapEitherT ClosureProjectionFailure id . runFlowT

-- |Lifts a compatibility operation into the closure monad.
liftCompatToClosure :: (Functor m, Monad m) => CompatM db m a -> ClosureM db m a
liftCompatToClosure =
  flow . bimapEitherT ClosureCompatibilityFailure id . runFlowT

-- |Creates a database with equality constraints over the specified type
--  variable.
doEqualityFor :: ( ConstraintDatabase db, MonadCReader db m, Functor m
                 , Applicative m )
              => FlowTVar -> OperationConstraint -> ConstraintHistory db
              -> ClosureM db m db
doEqualityFor a1 oc history =
  case a1 of
    GenFlowTVar _ _ ->
      error $ "How did a generated flow variable (" ++ display a1
        ++ ") appear as the upper bound of an integer comparison ("
        ++ display oc ++ ")?"
    FlowTVar x pc ->
      let a' = GenFlowTVar x pc in
      let labelTrue = LabelName (ComputedOrigin []) "True" in
      let labelFalse = LabelName (ComputedOrigin []) "False" in
      return $ fromList $ map (,history)
          [ WrapTypeConstraint $ TypeConstraint EmptyOnion a'
          , WrapTypeConstraint $ TypeConstraint (Label labelTrue a') a1
          , WrapTypeConstraint $ TypeConstraint (Label labelFalse a') a1
          ]
