{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, GADTs, DataKinds, KindSignatures, TypeFamilies, FlexibleContexts #-}

{-|
  This module defines PatBang's projection relation.
-}
module Language.PatBang.TypeSystem.Relations.Projection
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

import Language.PatBang.Ast
import Language.PatBang.Display
import Language.PatBang.Logging
import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Data.Projection
import Language.PatBang.TypeSystem.Fibrations
import Language.PatBang.TypeSystem.Monad.Trans.CReader
import Language.PatBang.TypeSystem.Monad.Trans.Flow
import Language.PatBang.TypeSystem.Types

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
      (var:_, fn) <- project proj a
      return (var, \f -> fn [f])
    ProjFun _ -> do
      (typ:_, f) <- project proj a
      return (typ, f)
    ProjPat _ -> do
      (typ:_, f) <- project proj a
      return (typ, f)
    ProjScape _ -> do
      (typ:_, f) <- project proj a
      return (typ, f)

-- |Performs single projection.  This obtains a prepared projection result
--  as well as the actual type.
projectSinglePrimResult :: ( ConstraintDatabase db, MonadCReader db m
                       , Functor m, Applicative m, Display db )
                    => Projector ProjPrimTag -> FlowTVar
                    -> ProjM db m (Bool, ProjectionResult db)
projectSinglePrimResult proj a = do
  (r,f) <- projectSingle proj a
  return (r, ProjectionResult proj a (ProjectionResultPrimForm r f))

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
              let ((x1,fib1),(x2,fib2)) = (r1,r2) in
              return (x2 || x1, Fibration lowerBound [fib1, fib2])
            ProjLabel _ _ ->
              let ((bs1,fn1),(bs2,fn2)) = (r1,r2) in
              -- Reverse order: first element is highest priority
              let bs = bs2 ++ bs1 in
              let fn fibs = Fibration lowerBound
                              [ fn1 $ drop (length bs2) fibs , fn2 fibs ] in
              return (bs, fn)
            ProjFun _ ->
              let ((ts1,fib1),(ts2,fib2)) = (r1,r2) in
              return (ts2 ++ ts1, Fibration lowerBound [fib1, fib2])
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
