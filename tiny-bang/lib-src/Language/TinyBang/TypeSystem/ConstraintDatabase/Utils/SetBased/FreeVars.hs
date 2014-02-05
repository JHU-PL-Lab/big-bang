{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

{-|
  This module defines a function which determines the free variables in a set of
  constraints.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.FreeVars
( findFreeVars

, createFindFreeVarsInstances
) where

import Control.Applicative
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH as TH

import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Types as TBT
import Language.TinyBang.Utils.TemplateHaskell.Reduce

findFreeVars :: (Reduce FindFreeVars a FindFreeVarsResult)
             => a -> Set TVar
findFreeVars cs =
  let (free,bound) = reduce FindFreeVars cs in free `Set.difference` bound

data FindFreeVars = FindFreeVars

type FindFreeVarsResult = (Set TVar, Set TVar) -- free/bound

$(concat <$> mapM (defineCatInstance [t|FindFreeVarsResult|] ''FindFreeVars)
    [ ''TypeOrVar
    ]
 )

$(defineCommonCatInstances [t|FindFreeVarsResult|] ''FindFreeVars)

instance (Reduce FindFreeVars db FindFreeVarsResult)
      => Reduce FindFreeVars (Constraint db) FindFreeVarsResult where
  reduce ffv c = case c of
    TypeConstraint _ t a ->
      reduce ffv t `mappend` (Set.empty, Set.singleton a)
    IntermediateConstraint _ a1 a2 ->
      reduce ffv a1 `mappend` (Set.empty, Set.singleton a2)
    ApplicationConstraint _ a1 a2 a3 ->
      reduce ffv a1 `mappend` reduce ffv a2 `mappend`
        (Set.empty, Set.singleton a3)
    BuiltinConstraint _ _ as a ->
      reduce ffv as `mappend` (Set.empty, Set.singleton a)
    InconsistencyConstraint _ _ ->
      (Set.empty, Set.empty)

instance (Reduce FindFreeVars db FindFreeVarsResult)
      => Reduce FindFreeVars (TBT.Type db) FindFreeVarsResult where
  reduce ffv t = case t of
    TEmptyOnion -> (Set.empty, Set.empty)
    TPrimitive _ -> (Set.empty, Set.empty)
    TLabel _ tov -> reduce ffv tov
    TRef a -> reduce ffv a
    TOnion tov1 tov2 -> reduce ffv tov1 `mappend` reduce ffv tov2
    TScape a' cs' a cs ->
      let (patFree, patBound) =
            reduce ffv a' `mappend` reduce ffv cs' in
      let (exprFree, exprBound) =
            reduce ffv a `mappend` reduce ffv cs in
      -- Calculate the variables which are free in this scape.
      ( ((patFree `Set.union` exprFree) `Set.difference` patBound)
          `Set.difference` exprBound
      , Set.empty)

instance Reduce FindFreeVars TVar FindFreeVarsResult where
  reduce _ a = (Set.singleton a, Set.empty)

-- |A routine to define an appropriate catamorphism for the wrapper of a set
--  of constraints.
createFindFreeVarsInstances :: Name -> Q [Dec]
createFindFreeVarsInstances =
  defineCatInstance [t|FindFreeVarsResult|] ''FindFreeVars
