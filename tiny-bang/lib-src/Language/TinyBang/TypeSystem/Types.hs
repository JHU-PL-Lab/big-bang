{-|
  This module contains the Haskell data types for basic types, type variables,
  and constraints in TinyBang.  This module's constructors overlap with the
  constructors in @Language.TinyBang.Ast@ and are expected to be qualified on
  import.
-}

module Language.TinyBang.TypeSystem.Types
( Type(..)
, PatternType(..)
, InnerPatternType(..)
, CellTVar(..)
, FlowTVar(..)
, AnyTVar(..)

, Constraint(..)
, IntermediateConstraint(..)
, TypeConstraint(..)
, ApplicationConstraint(..)
, OperationConstraint(..)
, CellCreationConstraint(..)
, CellLoadingConstraint(..)
, CellSettingConstraint(..)
, FinalConstraint(..)
, ImmutableConstraint(..)
, FlowConstraint(..)
, ExceptionConstraint(..)

, ConstraintHistory(..)
, SourceElement(..)

, ConstraintDatabase(..)
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.TypeSystem.Contours


-- * Types

-- |Represents TinyBang types.  Parametric in the type of constraint database
--  used to store scape constraints.
data Type db
  = Primitive A.PrimitiveType
  | EmptyOnion
  | Label A.LabelName CellTVar
  | Onion FlowTVar FlowTVar
  | OnionFilter FlowTVar A.OnionOp A.Projector
  | Scape PatternType FlowTVar db 
  deriving (Eq, Ord, Show)

-- |Represents TinyBang pattern types.
data PatternType
  = ValuePattern CellTVar InnerPatternType
  | ExnPattern CellTVar InnerPatternType
  deriving (Eq, Ord, Show)

-- |Represents TinyBang inner pattern types.
data InnerPatternType
  = PrimitivePattern A.PrimitiveType
  | LabelPattern A.LabelName CellTVar InnerPatternType
  | ConjunctivePattern InnerPatternType InnerPatternType
  | ScapePattern
  | EmptyOnionPattern
  deriving (Eq, Ord, Show)

-- |Represents cell type variables.
data CellTVar = CellTVar A.CellVar PossibleContour
  deriving (Eq, Ord, Show)

-- |Represents flow type variables.
data FlowTVar = FlowTVar A.FlowVar PossibleContour
  deriving (Eq, Ord, Show)
  
-- |A wrapper for any type variable.
data AnyTVar
  = SomeCellTVar CellTVar
  | SomeFlowTVar FlowTVar
  deriving (Eq, Ord, Show)
  

-- * Constraints

-- |Represents TinyBang type constraints.  Parametric in the type of database
--  which appears in the scape types within these constraints.
data Constraint db
  = WrapIntermediateConstraint IntermediateConstraint
  | WrapTypeConstraint (TypeConstraint db)
  | WrapApplicationConstraint ApplicationConstraint
  | WrapOperationConstraint OperationConstraint
  | WrapCellCreationConstraint CellCreationConstraint
  | WrapCellLoadingConstraint CellLoadingConstraint
  | WrapCellSettingConstraint CellSettingConstraint
  | WrapFinalConstraint FinalConstraint
  | WrapImmutableConstraint ImmutableConstraint
  | WrapFlowConstraint FlowConstraint
  | WrapExceptionConstraint ExceptionConstraint
  deriving (Eq, Ord, Show)

data IntermediateConstraint = IntermediateConstraint FlowTVar FlowTVar
  deriving (Eq, Ord, Show)
  
-- See notes/TypeConstraint-Foldable.txt for a limitation
data TypeConstraint db = TypeConstraint (Type db) FlowTVar
  deriving (Eq, Ord, Show)

data ApplicationConstraint = ApplicationConstraint FlowTVar FlowTVar FlowTVar
  deriving (Eq, Ord, Show)

data OperationConstraint
  = OperationConstraint FlowTVar A.BinaryOperator FlowTVar FlowTVar
  deriving (Eq, Ord, Show)

data CellCreationConstraint = CellCreationConstraint FlowTVar CellTVar
  deriving (Eq, Ord, Show)

data CellLoadingConstraint = CellLoadingConstraint CellTVar FlowTVar
  deriving (Eq, Ord, Show)

data CellSettingConstraint = CellSettingConstraint FlowTVar CellTVar
  deriving (Eq, Ord, Show)

data FinalConstraint = FinalConstraint CellTVar
  deriving (Eq, Ord, Show)

data ImmutableConstraint = ImmutableConstraint CellTVar
  deriving (Eq, Ord, Show)

data FlowConstraint = FlowConstraint FlowTVar A.FlowKind FlowTVar
  deriving (Eq, Ord, Show)

data ExceptionConstraint = ExceptionConstraint FlowTVar FlowTVar
  deriving (Eq, Ord, Show)


-- * Constraint history

data ConstraintHistory db
  = DerivedFromSource SourceElement
  | CompatibilityWiring -- TODO: more info?
  deriving (Eq, Ord, Show)

data SourceElement
  = ClauseElement A.Clause
  | PatternElement A.Pattern
  | QualifierElement A.CellQualifier
  deriving (Eq, Ord, Show)

instance A.HasOrigin SourceElement where
  originOf x = case x of
    ClauseElement cl -> A.originOf cl
    PatternElement pat -> A.originOf pat
    QualifierElement q -> A.originOf q


-- * Constraint databases

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
  -- |Finds all lower bounds for the provided type variable.
  getLowerBounds :: FlowTVar -> db -> Set (Type db)
  -- |Finds all cell assignment and construction bounds for the provided cell
  --  variable.
  getCellOrStoreBounds :: CellTVar -> db -> Set FlowTVar
  
  -- |Performs cell substitution on a database.  The provided map is keyed by
  --  the variables to replace and valued by their replacements.
  substituteCellVariables :: Map CellTVar CellTVar -> db -> db
  
  -- |Obtains the set of contours which appear in a database.
  getAllContours :: db -> Set Contour
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

-- * Typeclass instances

-- TODO: Display instances