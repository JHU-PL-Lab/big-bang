{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Language.PatBang.TypeSystem.Constraints
( Constraint(..)

, IntermediateConstraint(..)
, TypeConstraint(..)
, ApplicationConstraint(..)
, OperationConstraint(..)

, ConstraintWrappable(..)
) where

import qualified Language.PatBang.Ast as A
import Language.PatBang.Display
import Language.PatBang.TypeSystem.Types
import Language.PatBang.TypeSystem.Utils.DocumentContainer

-- * Constraint wrappers

-- |Represents PatBang type constraints.  Parametric in the type of database
--  which appears in the scape types within these constraints.
data Constraint db
  = WrapIntermediateConstraint IntermediateConstraint
  | WrapTypeConstraint (TypeConstraint db)
  | WrapApplicationConstraint ApplicationConstraint
  | WrapOperationConstraint OperationConstraint
  deriving (Eq, Ord, Show)

-- * Constraint types
  
data IntermediateConstraint = IntermediateConstraint FlowTVar FlowTVar
  deriving (Eq, Ord, Show)
  
-- See notes/TypeConstraint-Overlapping.txt for limitations
data TypeConstraint db = TypeConstraint (Type db) FlowTVar
  deriving (Eq, Ord, Show)

data ApplicationConstraint = ApplicationConstraint FlowTVar FlowTVar FlowTVar
  deriving (Eq, Ord, Show)

data OperationConstraint
  = OperationConstraint FlowTVar A.BinaryOperator FlowTVar FlowTVar
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

-- Display instance code

subdoc :: Doc
subdoc = text "<:"

instance (DocumentContainer db) => Display (Constraint db) where
  makeDoc c = case c of
    WrapIntermediateConstraint arg -> makeDoc arg
    WrapTypeConstraint arg -> makeDoc arg
    WrapApplicationConstraint arg -> makeDoc arg
    WrapOperationConstraint arg -> makeDoc arg

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

