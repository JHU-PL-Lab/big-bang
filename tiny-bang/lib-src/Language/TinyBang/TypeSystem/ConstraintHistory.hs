module Language.TinyBang.TypeSystem.ConstraintHistory
( ConstraintHistory(..)
, SourceElement(..)
, ClosureRule(..)
, ProjectionResult(..)
) where

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Types

data ConstraintHistory db
  = DerivedFromSource SourceElement
  | CompatibilityWiring -- TODO: more info?
  | DerivedFromClosure (ClosureRule db)
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
    
data ClosureRule db
  = TransitivityRule
      (TypeConstraint db) -- ^ The triggering constraint.
      IntermediateConstraint -- ^ The intermediate transitivity constraint.
  | IntegerOperationRule
      OperationConstraint -- ^ The triggering constraint.
      (ProjectionResult db) -- ^ The proof of integer projection for the left.
      (ProjectionResult db) -- ^ The proof of integer projection for the right.
  deriving (Eq, Ord, Show)
  
data ProjectionResult db
  = SingleProjectionResult A.Projector FlowTVar (Maybe (Type db)) (Fibration db)
  deriving (Eq, Ord, Show)
