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
, AnyVar(..)
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
) where

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.TypeSystem.Contours

data Type db
  = Primitive A.PrimitiveType
  | EmptyOnion
  | Label A.LabelName CellTVar
  | Onion FlowTVar FlowTVar
  | OnionFilter FlowTVar A.OnionOp A.Projector
  | Scape PatternType FlowTVar db 
  deriving (Eq, Ord, Show)

data PatternType
  = ValuePattern CellTVar InnerPatternType
  | ExnPattern CellTVar InnerPatternType
  deriving (Eq, Ord, Show)

data InnerPatternType
  = PrimitivePattern A.PrimitiveType
  | LabelPattern A.LabelName CellTVar InnerPatternType
  | ConjunctivePattern InnerPatternType InnerPatternType
  | ScapePattern
  | EmptyOnionPattern
  deriving (Eq, Ord, Show)

data CellTVar = CellTVar A.CellVar PossibleContour
  deriving (Eq, Ord, Show)

data FlowTVar = FlowTVar A.FlowVar PossibleContour
  deriving (Eq, Ord, Show)
  
data SomeTVar
  = SomeCellTVar CellTVar
  | SomeFlowTVar FlowTVar
  deriving (Eq, Ord, Show)

data AnyVar
  = SomeCellVar CellTVar
  | SomeFlowVar FlowTVar
  deriving (Eq, Ord, Show)
  
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
