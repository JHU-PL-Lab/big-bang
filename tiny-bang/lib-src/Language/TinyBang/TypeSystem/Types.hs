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
, FlowConstraint(..)
, TypeConstraint(..)
, ApplicationConstraint(..)
, OperationConstraint(..)
, CellCreationConstraint(..)
, CellLoadingConstraint(..)
, CellSettingConstraint(..)
, FinalConstraint(..)
, ImmutableConstraint(..)
, ExceptionConstraint(..)
) where

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.TypeSystem.Contours

data Type db
  = Primitive A.PrimitiveType
  | Label A.LabelName CellTVar
  | Onion FlowTVar FlowTVar
  | OnionFilter FlowTVar A.OnionOp
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

data AnyVar
  = SomeCellVar CellTVar
  | SomeFlowVar FlowTVar
  deriving (Eq, Ord, Show)
  
data Constraint db
  = WrapFlowConstraint FlowConstraint
  | WrapTypeConstraint (TypeConstraint db)
  | WrapApplicationConstraint ApplicationConstraint
  | WrapOperationConstraint OperationConstraint
  | WrapCellCreationConstraint CellCreationConstraint
  | WrapCellLoadingConstraint CellLoadingConstraint
  | WrapCellSettingConstraint CellSettingConstraint
  | WrapFinalConstraint FinalConstraint
  | WrapImmutableConstraint ImmutableConstraint
  | WrapExceptionConstraint ExceptionConstraint
  deriving (Eq, Ord, Show)
  
data FlowConstraint = FlowConstraint FlowTVar A.FlowKind FlowTVar
  deriving (Eq, Ord, Show)

-- See notes/TypeConstraint-Foldable.txt for a limitation
data TypeConstraint db = TypeConstraint (Type db) FlowTVar
  deriving (Eq, Ord, Show)

data ApplicationConstraint = ApplicationConstraint FlowTVar FlowTVar
  deriving (Eq, Ord, Show)

data OperationConstraint
  = OperationConstraint FlowTVar A.BinaryOperator FlowTVar
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

data ExceptionConstraint = ExceptionConstraint FlowTVar FlowTVar
  deriving (Eq, Ord, Show)
