{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- For the DocumentContainer instance

module Language.TinyBang.TypeSystem.ConstraintDatabase
( ConstraintDatabase(..)
) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Display hiding (empty)
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.TypeSystem.Utils.DocumentContainer

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
  getTypeConstraints :: db -> Set (TypeConstraint db)
  -- |Retrieves all integer calculation constraints from this database.  These
  --  are operation constraints for which the operator is either plus or minus.
  getIntegerCalculationConstraints :: db -> Set OperationConstraint
  -- |Retrieves all integer comparison constraints from this database.  These
  --  are operation constraints for which the operator is either greater-than
  --  or less-than.
  getIntegerOperationConstraints :: db -> Set OperationConstraint
  -- |Retrieves all equality constraints from this database.  These are
  --  operation constraints for which the operator is equality.
  getEqualityConstraints :: db -> Set OperationConstraint
  -- |Retrieves all cell reading constraints.
  getCellLoadingConstraints :: db -> Set CellLoadingConstraint
  -- |Retrieves all exception constraints.
  getExceptionConstraints :: db -> Set ExceptionConstraint
  
  -- |Finds all type constraints with the provided upper bound.
  getTypeConstraintsByUpperBound :: FlowTVar -> db -> Set (TypeConstraint db)
  -- |Finds all intermediate constraints with the provided lower bound.
  getIntermediateConstraintsByLowerBound :: FlowTVar -> db
                                         -> Set IntermediateConstraint
  -- |Retrieves all constraints which act as lower bounds for cells.
  getCellLowerBoundConstraints :: CellTVar -> db
                               -> Set CellLowerBoundingConstraint
  -- |Retrieves all flow constraints for a given lower bound and flow kind.
  getFlowConstraintsByLowerBound :: FlowTVar -> FlowKind -> db
                                 -> Set FlowConstraint
  
  -- |Performs cell substitution on a database.  The provided map is keyed by
  --  the variables to replace and valued by their replacements.
  substituteCellVariables :: Map CellTVar CellTVar -> db -> db
  
  -- |Obtains the set of contours which appear in a database.
  getAllContours :: db -> Set Contour
  -- |Instantiates contours on specific variables in a database.
  instantiateContours :: Contour -> db -> db
  -- |Performs contour replacement on the contents of a database.
  replaceContours :: Contour -> db -> db
  
  -- TODO: the rest of the interface
  
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
