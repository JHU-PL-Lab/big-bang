{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances #-}

{-|
  Specifies the typeclass interface for constraint databases and constraint
  database queries.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Interface
( ConstraintDatabase(..)
, ConstraintQuery(..)
) where

import Data.Foldable (Foldable)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Foldable as Foldable

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types

{-| Defines the typeclass of constraint databases.  A constraint database is a
    set of constraints instrumented with a series of operations related to the
    typechecking process of TinyBang.
    
    As constraints are added to a constraint database, the database takes
    responsibility for merging contours as per the TinyBang polymorphism model.
    In contrast to the specification, there is no single step during which a
    merge occurs; a @db@ value always maintains the invariant that contours are
    disjoint and properly merged.
-}
class (Eq db, Ord db, Monoid db) => ConstraintDatabase db where
  -- |An empty constraint database.
  empty :: db
  -- |Adds a new constraint to a database.
  add :: Constraint db -> db -> db
  -- |Unions two constraint databases.  If there is a performance difference
  --  based on the order of the arguments, it will generally be in favor of the
  --  first database being the larger one.
  union :: db -> db -> db
  
  -- |Queries the database.  The parameters of the query are canonicalized by
  --  this function; the caller is not responsible for that.
  query :: db -> ConstraintQuery db r -> Set r
  
  -- |Polyinstantiates contours on the free variables of this database except
  --  for those named in the provided bound variables set.  This can used for a
  --  capture-avoiding susbtitution.
  polyinstantiate :: Contour -> db -> db
  
  -- ### Performance functions by contract
  -- |Adds a new constraint to the database.  This function is potentially
  --  more efficient than 'add', but the caller is obligated to ensure that,
  --  in the provided constraint, every type variable either has no contour
  --  or has a contour which already appears in this constraint database.
  --  Failure to do so may result in this constraint database violating the
  --  invariant that all appearing contours are disjoint.
  addWithExistingContours :: Constraint db -> db -> db
  -- |Unions two constraint databases.  This function is potentially more
  --  efficient than 'union', but the caller must ensure that every type
  --  variable appearing in the second database either has no contour
  --  or has a contour which already appears in this constraint database.
  --  Failure to do so may result in this constraint database violating the
  --  invariant that all appearing contours are disjoint.
  unionWithExistingContours :: db -> db -> db

  -- ### Convenience functions
  empty = mempty
  union = mappend
  -- |Creates a singleton constraint database.  By default, this simply adds
  --  a constraint to an empty database.
  singleton :: Constraint db -> db
  singleton c = add c empty
  -- |Creates a constraint database from a list of constraint-history pairs.
  --  By default, this simply adds these constraints in order to an empty
  --  database.
  fromList :: (Foldable f) => f (Constraint db) -> db
  fromList = Foldable.foldr add empty
  
{-|
  A data type describing the forms of query which can be executed against a
  constraint database.  The queries are as follows:
    * @QueryAllConstraints@: all constraints appearing in the database.
    * @QueryAllTVars@: all type variables appearing anywhere in the database
    * @QueryAllFreeTVars@: all free type variables appearing in the database.
    * @QueryAllTypesLowerBoundingTVars@: constraints of the form "t <: a"
    * @QueryAllApplications@: constraints of the form "a1 a2 <: a3"
    * @QueryLowerBoundingTypesOfTVar@: types which lower bound the specified
                                       type variable.
    * @QueryLowerBoundingTVarsOfTVar@: type variabes which lower bound the
                                       specified type variable.
-}
data ConstraintQuery db r where
  QueryAllConstraints :: ConstraintQuery db (Constraint db)
  QueryAllTVars :: ConstraintQuery db TVar
  QueryAllFreeTVars :: ConstraintQuery db TVar
  QueryAllTypesLowerBoundingTVars :: ConstraintQuery db (Type db, TVar)
  QueryAllApplications :: ConstraintQuery db (TVar, TVar, TVar)
  QueryAllBuiltins :: ConstraintQuery db (BuiltinOp, [TVar], TVar)
  QueryAllInconsistencies :: ConstraintQuery db (Inconsistency db)
  QueryLowerBoundingTypesOfTVar :: TVar -> ConstraintQuery db (Type db)
  QueryUpperBoundingTVarsOfTVar :: TVar -> ConstraintQuery db TVar

