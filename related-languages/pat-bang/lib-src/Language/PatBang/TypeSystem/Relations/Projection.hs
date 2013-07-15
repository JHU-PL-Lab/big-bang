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
--  implementation differs from the specification in that
--    (1) non-contractive types are prohibited
--    (2) it accepts a /filtering/ fibration to control output
--
--  The resulting list of types is in reverse order; the first element of the
--  list is highest priority.
project :: forall db m (tag :: ProjectorTag).
           (Applicative m, ConstraintDatabase db, MonadCReader db m, Display db)
        => Projector tag
        -> FlowTVar
        -> Fibration db
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
              -> Fibration db
              -> ProjM db m (SingleProjection db tag)
projectSingle proj a ffib =
  case proj of
    ProjPrim _ _ -> project proj a ffib
    ProjLabel _ _ -> do
      (var:_, fn, ff:ffs) <- project proj a ffib
      return (var, \f -> fn (f:ffs), ff)
    ProjFun _ -> do
      (typ:_, f) <- project proj a ffib
      return (typ, f)
    ProjPat _ -> do
      (typ:_, f) <- project proj a ffib
      return (typ, f)
    ProjScape _ -> do
      (typ:_, fn, ffp:ffps) <- project proj a ffib
      return (typ, \f -> fn (f:ffps), ffp)

-- |Performs single projection.  This obtains a prepared projection result
--  as well as the actual type.
projectSinglePrimResult :: ( ConstraintDatabase db, MonadCReader db m
                       , Functor m, Applicative m, Display db )
                    => Projector ProjPrimTag -> FlowTVar -> Fibration db
                    -> ProjM db m (Bool, ProjectionResult db)
projectSinglePrimResult proj a ffib = do
  (r,f) <- projectSingle proj a ffib
  return (r, ProjectionResult proj a (ProjectionResultPrimForm r f))

-- |The *real* projection function.  This function includes an occurs check for
--  non-contractive onion types to prevent divergence.  It accepts a filtering
--  fibration which controls how projection expands.
projectVar :: forall db m (tag :: ProjectorTag).
              ( Applicative m, ConstraintDatabase db, MonadCReader db m
              , Display db)
           => OccursCheck
           -> Projector tag
           -> FlowTVar
           -> Fibration db
           -> ProjM db m (MultiProjection db tag)
projectVar check proj a ffib = do
  _debug $ "Checking projection of " ++ display proj ++ " from variable "
              ++ display a ++ " under filter " ++ display ffib
  answer <-
    case ffib of
      Unexpanded -> do
        TypeConstraint lowerBound _ <-
            flow $ lift $ getTypeConstraintsByUpperBound a <$> askDb
        projectType lowerBound $ unexpandedFibrationListFor lowerBound
      Fibration typ ffibs ->
        projectType typ ffibs
  _debug $ "Projection of " ++ display proj ++ " from variable "
              ++ display a ++ " gives " ++ projectionResultToString answer
  return answer
  where
    -- |The function used for projection from specific lower-bounding types.
    --  The input is the type from which to project and a list of appropriate
    --  filtering fibrations: one for each variable on the type.
    projectType :: Type db -> [Fibration db]
                -> ProjM db m (MultiProjection db tag)
    projectType lowerBound ffibs = do
      _debug $ "Checking projection of " ++ display proj ++ " from type "
                  ++ display lowerBound ++ " under filters " ++ display ffibs
      answer <- case (lowerBound, proj) of
        (Onion a1 a2, _) -> do
          let (ffib1, ffib2) = demand2F
          r1 <- projectRemembering a1 ffib1
          r2 <- projectRemembering a2 ffib2
          case proj of
            -- Reverse order in result lists: first element is highest priority
            ProjPrim _ _ ->
              let ((x1,fib1),(x2,fib2)) = (r1,r2) in
              return (x2 || x1, Fibration lowerBound [fib1, fib2])
            ProjLabel _ _ ->
              let ((bs1,fn1,ff1),(bs2,fn2,ff2)) = (r1,r2) in
              let fn fibs = Fibration lowerBound
                              [ fn1 $ drop (length bs2) fibs , fn2 fibs ] in
              return (bs2 ++ bs1, fn, ff2 ++ ff1)
            ProjFun _ ->
              let ((ts1,fib1),(ts2,fib2)) = (r1,r2) in
              return (ts2 ++ ts1, Fibration lowerBound [fib1, fib2])
            ProjPat _ ->
              let ((ts1,fib1),(ts2,fib2)) = (r1,r2) in
              return (ts2 ++ ts1, Fibration lowerBound [fib1, fib2])
            ProjScape _ ->
              let ((bs1,fn1,ff1),(bs2,fn2,ff2)) = (r1,r2) in
              let fn fibs = Fibration lowerBound
                              [ fn1 $ drop (length bs2) fibs , fn2 fibs ] in
              return (bs2 ++ bs1, fn, ff2 ++ ff1)
        (Primitive p, ProjPrim _ p') | p == p' ->
          demand0FM *> return (True, blankFibrationFor lowerBound)
        (_, ProjPrim _ _) ->
          demand0FM *> return (False, blankFibrationFor lowerBound)
        (Label n a', ProjLabel _ n') | n == n' ->
          let ffib1 = demand1F in
          let fibfn fibs =
                Fibration lowerBound
                  [fromMaybe Unexpanded $ listToMaybe fibs]
          in return ([a'], fibfn, [ffib1])
        (_, ProjLabel _ _) ->
          return ([], const $ blankFibrationFor lowerBound, [])
        (Function aa a' cs, ProjFun _) ->
          demand0FM *> return ([(aa,a',cs)], blankFibrationFor lowerBound)
        (_, ProjFun _) ->
          demand0FM *> return ([], blankFibrationFor lowerBound)
        (Pattern bs tpat, ProjPat _) ->
          demand0FM *> return ([(bs,tpat)], blankFibrationFor lowerBound)
        (_, ProjPat _) ->
          demand0FM *> return ([], blankFibrationFor lowerBound)
        (Scape a' a'', ProjScape _) ->
          let (ffib1, ffib2) = demand2F in
          let fibfn fibps =
                let (fib1,fib2) = fromMaybe (Unexpanded, Unexpanded) $
                                      listToMaybe fibps in
                Fibration lowerBound [fib1, fib2]
          in return ([(a',a'')],fibfn,[(ffib1,ffib2)])
        (_, ProjScape _) ->
          demand0FM *> return ([], const $ blankFibrationFor lowerBound, [])
      _debug $ "Checked projection of " ++ display proj ++ " from type "
                ++ display lowerBound ++ " under filters " ++ display ffibs
                ++ " and received " ++ projectionResultToString answer
      {- TODO: result debugging messages -}
      return answer
      where
        projectRemembering :: FlowTVar -> Fibration db
                           -> ProjM db m (MultiProjection db tag)
        projectRemembering a' ffib' =
          let (aset,alist) = check in
          if Set.member a' aset
            then lift $ left $
                    NonContractiveType (SomeProjector proj) lowerBound $
                      reverse alist
            else projectVar ((Set.insert a' *** (a':)) check) proj a' ffib'
        demandNF :: Int -> a -> a
        demandNF n x =
          if length ffibs /= n
            then error $ "Invalid fibration list for type "
                            ++ display lowerBound
                            ++ ": " ++ display ffibs
            else x
        demand0F = demandNF 0 ()
        demand0FM :: ProjM db m ()
        demand0FM = return demand0F
        demand1F = demandNF 1 $ ffibs !! 0
        demand2F = demandNF 2 $ (ffibs !! 0, ffibs !! 1)
    projectionResultToString res =
      case (proj,res) of
        (ProjPrim _ _, p) -> display p
        (ProjLabel _ _, (x1,x2,x3)) -> display (x1, x2 x3)
        (ProjFun _, p) -> display p
        (ProjPat _, p) -> display p
        (ProjScape _, (x1,x2,x3)) -> display (x1, x2 x3)
        