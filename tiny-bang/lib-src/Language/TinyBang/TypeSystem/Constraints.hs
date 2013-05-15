{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Language.TinyBang.TypeSystem.Constraints
( Constraint(..)
, CellLowerBoundingConstraint(..)

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

, ConstraintWrappable(..)
, lowerBoundOf
) where

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.Display
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.TypeSystem.Utils.DocumentContainer

-- * Constraint wrappers

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

data CellLowerBoundingConstraint
  = CellCreationLowerBoundingConstraint CellCreationConstraint
  | CellSettingLowerBoundingConstraint CellSettingConstraint
  deriving (Eq, Ord, Show)
  
-- * Constraint types
  
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
  
-- * Convenience definitions
  
-- |A convenience typeclass which intelligently wraps contraints based on their
--  type.
class ConstraintWrappable db a where
  cwrap :: a -> Constraint db

instance ConstraintWrappable db IntermediateConstraint where
  cwrap = WrapIntermediateConstraint
instance ConstraintWrappable db (TypeConstraint db) where
  cwrap = WrapTypeConstraint
instance ConstraintWrappable db ApplicationConstraint where
  cwrap = WrapApplicationConstraint
instance ConstraintWrappable db OperationConstraint where
  cwrap = WrapOperationConstraint
instance ConstraintWrappable db CellCreationConstraint where
  cwrap = WrapCellCreationConstraint
instance ConstraintWrappable db CellLoadingConstraint where
  cwrap = WrapCellLoadingConstraint
instance ConstraintWrappable db CellSettingConstraint where
  cwrap = WrapCellSettingConstraint
instance ConstraintWrappable db FinalConstraint where
  cwrap = WrapFinalConstraint
instance ConstraintWrappable db ImmutableConstraint where
  cwrap = WrapImmutableConstraint
instance ConstraintWrappable db FlowConstraint where
  cwrap = WrapFlowConstraint
instance ConstraintWrappable db ExceptionConstraint where
  cwrap = WrapExceptionConstraint
  
lowerBoundOf :: CellLowerBoundingConstraint -> FlowTVar
lowerBoundOf c = case c of
  CellCreationLowerBoundingConstraint (CellCreationConstraint a _) -> a
  CellSettingLowerBoundingConstraint (CellSettingConstraint a _) -> a

-- Display instance code

subdoc :: Doc
subdoc = text "<:"

instance (DocumentContainer db) => Display (Constraint db) where
  makeDoc c = case c of
    WrapIntermediateConstraint arg -> makeDoc arg
    WrapTypeConstraint arg -> makeDoc arg
    WrapApplicationConstraint arg -> makeDoc arg
    WrapOperationConstraint arg -> makeDoc arg
    WrapCellCreationConstraint arg -> makeDoc arg
    WrapCellLoadingConstraint arg -> makeDoc arg
    WrapCellSettingConstraint arg -> makeDoc arg
    WrapFinalConstraint arg -> makeDoc arg
    WrapImmutableConstraint arg -> makeDoc arg
    WrapFlowConstraint arg -> makeDoc arg
    WrapExceptionConstraint arg -> makeDoc arg

instance Display IntermediateConstraint where
  makeDoc (IntermediateConstraint a a') = makeDoc a <+> subdoc <+> makeDoc a'

instance (DocumentContainer db) => Display (TypeConstraint db) where
  makeDoc (TypeConstraint t a) = makeDoc t <+> subdoc <+> makeDoc a
  
instance Display ApplicationConstraint where
  makeDoc (ApplicationConstraint a a' a'') =
    makeDoc a <+> makeDoc a' <+> subdoc <+> makeDoc a''

instance Display OperationConstraint where
  makeDoc (OperationConstraint a op a' a'') =
    makeDoc a <+> makeDoc op <+> makeDoc a' <+> subdoc <+> makeDoc a''

instance Display CellCreationConstraint where
  makeDoc (CellCreationConstraint a b) =
    text "cell" <+> makeDoc a <+> subdoc <+> makeDoc b

instance Display CellLoadingConstraint where
  makeDoc (CellLoadingConstraint b a) =
    text "load" <+> makeDoc b <+> subdoc <+> makeDoc a

instance Display CellSettingConstraint where
  makeDoc (CellSettingConstraint a b) =
    text "store" <+> makeDoc a <+> subdoc <+> makeDoc b

instance Display FinalConstraint where
  makeDoc (FinalConstraint b) = text "final" <+> makeDoc b

instance Display ImmutableConstraint where
  makeDoc (ImmutableConstraint b) = text "immut" <+> makeDoc b

instance Display FlowConstraint where
  makeDoc (FlowConstraint a k a') =
    makeDoc a <+> makeDoc k <> subdoc <+> makeDoc a'                                     

instance Display ExceptionConstraint where
  makeDoc (ExceptionConstraint a a') =
    text "exn" <+> makeDoc a <+> subdoc <+> makeDoc a'

instance Display CellLowerBoundingConstraint where
  makeDoc arg = case arg of
    CellCreationLowerBoundingConstraint c -> makeDoc c
    CellSettingLowerBoundingConstraint c -> makeDoc c
