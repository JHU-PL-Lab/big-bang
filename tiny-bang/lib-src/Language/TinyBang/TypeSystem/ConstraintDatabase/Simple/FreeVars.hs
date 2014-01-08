{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

{-|
  This module defines a function which determines the free variables in a set of
  constraints.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.FreeVars
( findFreeVars
) where

import Control.Applicative
import Data.Monoid
import Data.Set (Set)

import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Data
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.TemplateHaskell.Reduce

findFreeVars :: SimpleConstraintDatabase -> Set TVar
findFreeVars cs =
  snd $ reduce FindFreeVars cs

data FindFreeVars = FindFreeVars

type FindFreeVarsResult = (Set TVar, Set TVar) -- free/bound

$(concat <$> mapM (defineCatInstance [t|FindFreeVarsResult|] ''FindFreeVars)
    [ ''TypeOrVar
    ]
 )

$(defineCommonCatInstances [t|FindFreeVarsResult|] ''FindFreeVars)

instance Reduce FindFreeVars SimpleConstraintDatabase FindFreeVarsResult where
  reduce ffv db =
    let (free,bound) = reduce ffv $ unSimpleConstraintDatabase db in
    (free `Set.difference` bound, Set.empty)

instance Reduce FindFreeVars (Constraint SimpleConstraintDatabase)
            FindFreeVarsResult where
  reduce ffv c = case c of
    TypeConstraint _ t a ->
      reduce ffv t `mappend` (Set.empty, Set.singleton a)
    IntermediateConstraint _ a1 a2 ->
      reduce ffv a1 `mappend` (Set.empty, Set.singleton a2)
    ApplicationConstraint _ a1 a2 a3 ->
      reduce ffv a1 `mappend` reduce ffv a2 `mappend`
        (Set.empty, Set.singleton a3)
    InconsistencyConstraint _ _ ->
      (Set.empty, Set.empty)

instance Reduce FindFreeVars (Type SimpleConstraintDatabase)
            FindFreeVarsResult where
  reduce ffv t = case t of
    TEmptyOnion -> (Set.empty, Set.empty)
    TPrimitive _ -> (Set.empty, Set.empty)
    TLabel _ tov -> reduce ffv tov
    TOnion tov1 tov2 -> reduce ffv tov1 `mappend` reduce ffv tov2
    TScape a' cs' a cs ->
      let (patFree, patBound) =
            reduce ffv a' `mappend`
              reduce ffv (unSimpleConstraintDatabase cs') in
      let (exprFree, exprBound) =
            reduce ffv a `mappend` reduce ffv cs in
      -- Calculate the variables which are free in this scape.
      ( ((patFree `Set.union` exprFree) `Set.difference` patBound)
          `Set.difference` exprBound
      , Set.empty)

instance Reduce FindFreeVars TVar FindFreeVarsResult where
  reduce _ a = (Set.singleton a, Set.empty)
