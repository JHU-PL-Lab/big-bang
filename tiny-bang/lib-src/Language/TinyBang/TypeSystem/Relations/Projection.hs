{-# LANGUAGE ScopedTypeVariables #-}

{-|
  This module defines TinyBang's projection relation.
-}
module Language.TinyBang.TypeSystem.Relations.Projection
( ProjectionError(..)
, project
) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad.Reader
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.Flow
import Language.TinyBang.TypeSystem.Types

-- |A data type describing errors projection.
data ProjectionError db
  = NonContractiveType Projector (Type db) [FlowTVar]

-- |An alias for the projection monad.
type ProjM db m a = FlowT (EitherT (ProjectionError db) m) a

-- |Computes the possible projections of a type variable and projector.  This
--  operation occurs in the context of a constraint database.  This
--  implementation differs from the specification in that non-contractive types
--  are prohibited.
project :: forall db m.
           (Applicative m, ConstraintDatabase db, MonadCReader db m)
        => Projector
        -> FlowTVar
        -> FlowT (EitherT (ProjectionError db) m) ([Type db], Fibration db)
project = projectVar (Set.empty,[])

type OccursCheck = (Set FlowTVar, [FlowTVar])

-- |The *real* projection function.  This function includes an occurs check for
--  non-contractive onion types to prevent divergence.
projectVar :: forall db m.
              (Applicative m, ConstraintDatabase db, MonadCReader db m)
           => OccursCheck
           -> Projector
           -> FlowTVar
           -> ProjM db m ([Type db], Fibration db)
projectVar check proj a = do
  lowerBound <- (flow $
                    (lift $
                        (askLowerBounds a :: m (Set (Type db)))
                      :: EitherT (ProjectionError db) m (Set (Type db)))
                  :: ProjM db m (Type db))
  projectType lowerBound
  where
    projectType :: Type db -> ProjM db m ([Type db], Fibration db)
    projectType lowerBound =
      case (lowerBound, proj) of
        (Primitive p, ProjPrim _ p') | p == p' ->
          return ([lowerBound], Fibration lowerBound [])
        (Label n _, ProjLabel _ n') | n == n' ->
          return ([lowerBound], Fibration lowerBound [Unexpanded])
        (Scape _ _ _, ProjFun _) ->
          return ([lowerBound], Fibration lowerBound [])
        (Onion a1 a2, _) -> do
          (p1, fib1) <- projectRemembering a1
          (p2, fib2) <- projectRemembering a2
          return (p1 ++ p2, Fibration lowerBound [fib1, fib2])
        
        --(Primitive p, ProjPrim _ p') | p /= p' -> return ([], Unexpanded)
        --(Label n _, ProjLabel n') | n /= n' -> return ([], Unexpanded)
      where
        projectRemembering :: FlowTVar
                           -> ProjM db m ([Type db], Fibration db)
        projectRemembering a' =
          let (aset,alist) = check in
          if Set.member a' aset
            then lift $ left $
                    NonContractiveType proj lowerBound $ reverse alist
            else projectVar ((Set.insert a' *** (a':)) check) proj a'
