{-# LANGUAGE TemplateHaskell #-}

module Language.TinyBang.TypeSystem.Simple.Polymorphism
( tFresh
, polyinstFnForConstraintSet
, polyinstFnForConstraintSetWithContour
, polyinstFn
, polyinstFnWithContour
, restrictPolyinstFn
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Simple.Data
import Language.TinyBang.TypeSystem.Simple.Variables
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

-- |Performs freshening only on the upper bounding variables appearing within
--  a constraint set.  This is used to freshen bindings during application (as
--  the lower bounds are derived from arguments which need not be freshened).
tFresh :: (TVar -> TVar) -> ConstraintSet -> ConstraintSet
tFresh f (ConstraintSet cs) = ConstraintSet $
  Set.map freshenUpperBound cs
  where
    freshenUpperBound :: Constraint -> Constraint
    freshenUpperBound c = case c of
      LowerBoundConstraint rt a -> LowerBoundConstraint rt $ f a
      IntermediateConstraint a' a -> IntermediateConstraint a' $ f a
      ApplicationConstraint a0 a1 a2 -> ApplicationConstraint a0 a1 $ f a2
      BuiltinOpConstraint op as a -> BuiltinOpConstraint op as $ f a

-- |Creates a polyinstantiation function for a given constraint set and call
--  site.  This function is appropriately restricted to only instantiate the
--  variables which are bound by that set.
polyinstFnForConstraintSet :: ConstraintSet -> TVar -> TVar -> TVar
polyinstFnForConstraintSet cs a =
  let f = polyinstFn a in
  let boundVars = findTVarsDefinedBy cs in
  restrictPolyinstFn f boundVars

-- |Creates a polyinstantiation function for a given constraint set and call
--  site.  This function is appropriately restricted to only instantiate the
--  variables which are bound by that set.
polyinstFnForConstraintSetWithContour :: ConstraintSet -> Contour
                                      -> TVar -> TVar
polyinstFnForConstraintSetWithContour cs cntr =
  let f = polyinstFnWithContour cntr in
  let boundVars = findTVarsDefinedBy cs in
  _debugI (display $ text "Bound vars of" <+> makeDoc cs <+> text "are" <+> makeDoc boundVars) $
  restrictPolyinstFn f boundVars

polyinstFn :: TVar -> TVar -> TVar
polyinstFn (TVar a (PossibleContour pc)) =
  case pc of
    Nothing ->
      error "Attempted to polyinstantiate using non-contoured basis variable!"
    Just cntr ->
      let newCntr = extend a cntr in
      -- TODO: FIXME: This should be computing the new contour based on the call
      --              site, not just using the old contour.
      polyinstFnWithContour newCntr

polyinstFnWithContour :: Contour -> TVar -> TVar
polyinstFnWithContour cntr =
  \a'@(TVar x (PossibleContour pc')) ->
    case pc' of
      Nothing -> TVar x $ PossibleContour $ Just cntr
      Just _ ->
        error $ "Attempted to polyinstantiate a contoured variable: " ++
                display a'

restrictPolyinstFn :: (TVar -> TVar) -> Set TVar -> TVar -> TVar
restrictPolyinstFn f as a = if a `Set.member` as then f a else a

