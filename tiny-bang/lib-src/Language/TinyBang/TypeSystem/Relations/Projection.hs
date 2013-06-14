{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, GADTs, DataKinds, KindSignatures, TypeFamilies, FlexibleContexts #-}

{-|
  This module defines TinyBang's projection relation.
-}
module Language.TinyBang.TypeSystem.Relations.Projection
( ProjectionError(..)
, ProjM
, project
, projectSingle
, projectSinglePrimResult
) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Display
import Language.TinyBang.Logging
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Data.Projection
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.Flow
import Language.TinyBang.TypeSystem.Types

$(loggingFunctions)

-- |A data type describing errors projection.
data ProjectionError db
  = NonContractiveType AnyProjector (Type db) [FlowTVar]
  deriving (Eq, Ord, Show)
instance (ConstraintDatabase db, Display db)
      => Display (ProjectionError db) where
  makeDoc (NonContractiveType proj typ vars) =
    text "NonContractiveType" <+> makeDoc proj <+> makeDoc typ <+> makeDoc vars
    
-- |An alias for the projection monad.
type ProjM db m = FlowT (EitherT (ProjectionError db) m)

-- |Computes the possible projections of a type variable and projector.  This
--  operation occurs in the context of a constraint database.  This
--  implementation differs from the specification in that non-contractive types
--  are prohibited.
--
--  The resulting list of types is in reverse order; the first element of the
--  list is highest priority.
project :: forall db m (tag :: ProjectorTag).
           (Applicative m, ConstraintDatabase db, MonadCReader db m, Display db)
        => Projector tag
        -> FlowTVar
        -> ProjM db m (MultiProjection db tag) 
project = projectVar (Set.empty,[])

type OccursCheck = (Set FlowTVar, [FlowTVar])

-- TODO: rewrite projectSingle for efficiency.  It should use a different
-- exploration tactic for onions so as to avoid exploring the left side of a
-- tree if it doesn't need it.

-- |Computes the possible single projections of a type variable and projector.
--  See @project@ for more information.
projectSingle :: forall db m (tag :: ProjectorTag).
                 ( Applicative m, ConstraintDatabase db, MonadCReader db m
                 , Display db)
              => Projector tag
              -> FlowTVar
              -> ProjM db m (SingleProjection db tag)
projectSingle proj a =
  case proj of
    ProjPrim _ _ -> project proj a
    ProjLabel _ _ -> do
      (var:vars, fn) <- project proj a
      return (var, \f -> fn [f])
    ProjFun _ -> project proj a

-- |Performs single projection.  This obtains a prepared projection result
--  as well as the actual type.
projectSinglePrimResult :: ( ConstraintDatabase db, MonadCReader db m
                       , Functor m, Applicative m, Display db )
                    => Projector ProjPrimTag -> FlowTVar
                    -> ProjM db m (Bool, SingleProjectionResult db)
projectSinglePrimResult proj a = do
  (r,f) <- projectSingle proj a
  return (r, SingleProjectionResult (SomeProjector proj) a (r,f))

-- |The *real* projection function.  This function includes an occurs check for
--  non-contractive onion types to prevent divergence.
projectVar :: forall db m (tag :: ProjectorTag).
              ( Applicative m, ConstraintDatabase db, MonadCReader db m
              , Display db)
           => OccursCheck
           -> Projector tag
           -> FlowTVar
           -> ProjM db m (MultiProjection db tag)
projectVar check proj a = do
  _debug $ "Checking projection of " ++ display proj ++ " from variable "
              ++ display a
  TypeConstraint lowerBound _ <-
      flow $ lift $ getTypeConstraintsByUpperBound a <$> askDb
  answer <- projectType lowerBound
  {- TODO: result debugging messages -}
  {-
  _debug $ "Projection of " ++ display proj ++ " from variable "
              ++ display a ++ " gives " ++ display ts ++ " at fibration "
              ++ display fib
  -}
  return answer
  where
    projectType :: Type db -> ProjM db m (MultiProjection db tag)
    projectType lowerBound =
      case (lowerBound, proj) of
        (Onion a1 a2, _) -> do
          r1 <- projectRemembering a1
          r2 <- projectRemembering a2
          case proj of
            ProjPrim _ _ ->
              let ((p1,fib1),(p2,fib2)) = (r1,r2) in
              return (p2 && p1, Fibration lowerBound [fib1, fib2])
--          (p1, fib1) <- projectRemembering a1
--          (p2, fib2) <- projectRemembering a2
--          -- Reverse order: first element is highest priority
--          return (p2 ++ p1, Fibration lowerBound [fib1, fib2])
        (OnionFilter a1 (OpOnionSub _) proj', _)
            | SomeProjector proj /= proj' ->
          projectRemembering a1
        (OnionFilter a1 (OpOnionProj _) proj', _)
            | SomeProjector proj == proj' ->
          projectRemembering a1
        (Primitive p, ProjPrim _ p') | p == p' ->
          return (True, Fibration lowerBound [])
        (_, ProjPrim _ _) ->
          return (False, Fibration lowerBound [])
        (Label n b, ProjLabel _ n') | n == n' ->
          let fibfn fibs =
                Fibration lowerBound
                  [fromMaybe Unexpanded $ listToMaybe fibs]
          in return ([b], fibfn)
        (_, ProjLabel _ _) ->
          return ([], const $ blankFibrationFor lowerBound)
        (Scape _ _ _, ProjFun _) ->
          return (True, Fibration lowerBound [])
        (_, ProjFun _) ->
          return (False, Fibration lowerBound [])
      where
        projectRemembering :: FlowTVar
                           -> ProjM db m (MultiProjection db tag)
        projectRemembering a' =
          let (aset,alist) = check in
          if Set.member a' aset
            then lift $ left $
                    NonContractiveType (SomeProjector proj) lowerBound $
                      reverse alist
            else projectVar ((Set.insert a' *** (a':)) check) proj a'
