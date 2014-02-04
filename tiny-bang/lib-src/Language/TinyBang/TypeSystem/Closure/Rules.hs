{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances, GeneralizedNewtypeDeriving  #-}
{-|
  This module implements the TinyBang constraint closure function.
-}
module Language.TinyBang.TypeSystem.Closure.Rules
( calculateClosure
) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Closure.Basis
import Language.TinyBang.TypeSystem.Closure.Builtins
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
  {- TODO: global debugging mode
    In general, we are conflating debug /logging/ and debug /mode/.  If we were
    in debug /mode/, we would look at the contours present in the constraint set
    and verify that they were disjoint; if they aren't disjoint, we would /log/
    an error.  But this is an expensive computation; in this way, the debug mode
    mentioned here would be similar to Java's "enable assertions".  So yeah; we
    should have a one of those.
  -}
  bracketLog _debugI
    (display $
      text "Calculating closure step for:" </> align (indent 2 $ makeDoc db) </>
      text "Closure step contours present:" </> align (indent 2 $
        sepDoc (char ',') $ map makeDoc $ catMaybes $ Set.toList $
          Set.map contourOfVar $ query db QueryAllTVars))
    (\result -> display $ text "Finished closure step:" <+> makeDoc result) $
    foldr CDb.union db $ concatMap (`runClosureStepM` db) rules
  where
    rules :: [ClosureStepM db db]
    rules =
      [ closeTransitivity
      , closeApplication
      , closeBuiltin
      ]

closeTransitivity :: (ConstraintDatabase db, Display db) => ClosureStepM db db
closeTransitivity = bracketLogM _debugI
  "Calculating transitivity closure"
  (\res -> display $ text "Transitivity results:" <+> makeDoc res) $
  do
    (t,a1) <- join $ choose <$> queryDb QueryAllTypesLowerBoundingTVars
    a2 <- join $ choose <$> queryDb (QueryUpperBoundingTVarsOfTVar a1)
    let h = DerivedFromClosure $ TransitivityRule t a1 a2
    return $ CDb.singleton $ t <: a2 .: h

closeApplication :: forall db. (ConstraintDatabase db, Display db)
                 => ClosureStepM db db
closeApplication = bracketLogM _debugI
  "Calculating application closure"
  (\res -> display $ text "Application results:" <+> makeDoc res) $
  do
    (a0,a1,a2) <-
      bracketLogM (maybeLog _debugI) Nothing
        (\(a0,a1,a2) -> Just $ display $
                          text "Found call site" <+> makeDoc a2 <+>
                          text "with function" <+> makeDoc a0 <+>
                          text "and argument" <+> makeDoc a1) $
        join $ choose <$> queryDb QueryAllApplications
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

closeBuiltin :: forall db. (ConstraintDatabase db, Display db)
             => ClosureStepM db db
closeBuiltin = bracketLogM _debugI
  "Calculating built-in closure"
  (\res -> display $  text "Built-in operation results:" <+> makeDoc res) $
  do
    (op,as,a) <- join $ choose <$> queryDb QueryAllBuiltins
    let h = DerivedFromClosure $ BuiltinRule op as a
    -- NOTE: Technically, the specification allows the built-in operation to
    --       completely replace the constraint set, but this function returns
    --       a set of constraints to union into the old constraint set.  That
    --       said, the builtins currently just union new constraints in, so this
    --       is fine at the moment.
    (db', mt) <- builtinTypeEval h op as a
    case mt of
      Nothing -> return db'
      Just t -> return $ CDb.union db' $ CDb.singleton $ t <: a .: h
