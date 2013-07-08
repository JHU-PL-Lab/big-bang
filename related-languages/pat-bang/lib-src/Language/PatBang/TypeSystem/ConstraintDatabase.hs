{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- For the DocumentContainer instance

module Language.PatBang.TypeSystem.ConstraintDatabase
( ConstraintDatabase(..)
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Display hiding (empty)
import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Contours
import Language.PatBang.TypeSystem.Types
import Language.PatBang.TypeSystem.Utils.DocumentContainer

-- |A typeclass defining the interface for constraint databases.
class (Eq db) => ConstraintDatabase db where
  -- |Obtains an empty constraint database.
  empty :: db
  -- |Adds a new constraint to a database.
  add :: Constraint db -> ConstraintHistory db -> db -> db
  -- |Unions two databases.  If there is a performance difference between the
  --  ordering of the arguments, it will generally be in favor of the first
  --  database being the larger one.
  union :: db -> db -> db

  -- |Retrieves all constraints stored in this database.
  getAllConstraints :: db -> Set (Constraint db)
  -- |Retrieves all type constraints from this database.
  getTypeConstraints :: db -> [TypeConstraint db]
  -- |Retrieves all integer calculation constraints from this database.  These
  --  are operation constraints for which the operator is either plus or minus.
  getIntegerCalculationConstraints :: db -> [OperationConstraint]
  -- |Retrieves all integer comparison constraints from this database.  These
  --  are operation constraints for which the operator is either greater-than
  --  or less-than.
  getIntegerOperationConstraints :: db -> [OperationConstraint]
  -- |Retrieves all equality constraints from this database.  These are
  --  operation constraints for which the operator is equality.
  getEqualityConstraints :: db -> [OperationConstraint]
  -- |Retrieves all application constraints.
  getApplicationConstraints :: db -> [ApplicationConstraint]
  
  -- |Finds all type constraints with the provided upper bound.
  getTypeConstraintsByUpperBound :: FlowTVar -> db -> [TypeConstraint db]
  -- |Finds all intermediate constraints with the provided lower bound.
  getIntermediateConstraintsByLowerBound :: FlowTVar -> db
                                         -> [IntermediateConstraint]
                                      
  -- |Retrieves from a database the set of all type variables bound by its
  --  constraints.
  boundVariables :: db -> Set FlowTVar
  
  -- |Obtains the set of contours which appear in a database.
  getAllContours :: db -> Set Contour
  -- |Instantiates contours on the free variables of this database except for
  --  those named in the provided bound variables set.
  instantiateContours :: Set FlowTVar -> Contour -> db -> db
  -- |Performs contour replacement on the contents of a database.
  replaceContours :: Contour -> db -> db
  
  -- ### Convenience functions
  -- |Creates a singleton constraint database.  By default, this simply adds
  --  a constraint to an empty database.
  singleton :: Constraint db -> ConstraintHistory db -> db
  singleton c h = add c h empty
  -- |Creates a constraint database from a list of constraint-history pairs.
  --  By default, this simply adds these constraints in order to an empty
  --  database.
  fromList :: [(Constraint db, ConstraintHistory db)] -> db
  fromList = foldr (uncurry add) empty

  
instance (ConstraintDatabase db) => DocumentContainer db where
  getContainedDocuments = map makeDoc . Set.toList . getAllConstraints
