module Language.TinyBang.TypeSystem.ConstraintHistory
( ConstraintHistory(..)
, SourceElement(..)
, ClosureRule(..)
, SingleProjectionResult(..)
, MultiProjectionResult(..)
, ApplicationCompatibilityResult(..)
) where

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.Display
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Data.Compatibility
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.TypeSystem.Utils.DocumentContainer

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
      (SingleProjectionResult db)
        -- ^ The proof of integer projection for the left.
      (SingleProjectionResult db)
        -- ^ The proof of integer projection for the right.
  | IntegerCalculationRule
      OperationConstraint -- ^ The triggering constraint.
      (SingleProjectionResult db)
        -- ^ The proof of integer projection for the left.
      (SingleProjectionResult db)
        -- ^ The proof of integer projection for the right.
  | EqualityRule
      OperationConstraint -- ^ The triggering constraint.
  | ApplicationRule
      ApplicationConstraint -- ^ The triggering constraint.
      (MultiProjectionResult db) -- ^ The projection of scapes
      (ApplicationCompatibilityResult db) -- ^ Argument compatibility
      Contour -- ^ The contour generated for the application
  | CellPropagationRule
      CellLoadingConstraint -- ^ The triggering constraint
      CellLowerBoundingConstraint -- ^ The constraint which sets the cell lower
                                  --   bound.
  | ExceptionPropagationRule
      ExceptionConstraint -- ^ The triggering constraint.
      FlowConstraint -- ^ The flow constraint over which the exception moves.
  | ExceptionPassRule
      ApplicationConstraint -- ^ The triggering constraint.
      ExceptionConstraint -- ^ The exception constraint describing the argument
      (MultiProjectionResult db) -- ^ The projection of scapes
      (ApplicationCompatibilityResult db) -- ^ Argument compatibility      
  | ExceptionCatchRule
      ApplicationConstraint -- ^ The triggering constraint.
      ExceptionConstraint -- ^ The exception constraint describing the argument
      (MultiProjectionResult db) -- ^ The projection of scapes
      (ApplicationCompatibilityResult db) -- ^ Argument compatibility
      Contour -- ^ The contour generated for the application
  deriving (Eq, Ord, Show)
  
data SingleProjectionResult db
  = SingleProjectionResult A.Projector FlowTVar (Maybe (Type db)) (Fibration db)
  deriving (Eq, Ord, Show)

data MultiProjectionResult db
  = MultiProjectionResult A.Projector FlowTVar [Type db] (Fibration db)
  deriving (Eq, Ord, Show)

-- TODO: deep history on this?
data ApplicationCompatibilityResult db
  = ApplicationCompatibilityResult
      CompatibilityArgument -- ^ The argument
      [Type db] -- ^ The scape types
      (Maybe (FlowTVar, db))
        -- ^ The compatibility output variable and constraints (if successful)
      (Fibration db)
        -- ^ The fibration of the compatibility
  deriving (Eq, Ord, Show)
  
instance (Display db, DocumentContainer db)
      => Display (SingleProjectionResult db) where
  makeDoc (SingleProjectionResult proj a mt fib) =
    text "SingleProjectionResult" <+> parens (makeDoc proj)
      <+> parens (makeDoc a) <+> parens (makeDoc mt) <+> parens (makeDoc fib)

instance (Display db, DocumentContainer db)
      => Display (MultiProjectionResult db) where
  makeDoc (MultiProjectionResult proj a ts fib) =
    text "MultiProjectionResult" <+> parens (makeDoc proj)
      <+> parens (makeDoc a) <+> parens (makeDoc ts) <+> parens (makeDoc fib)

instance (Display db, DocumentContainer db)
      => Display (ApplicationCompatibilityResult db) where
  makeDoc (ApplicationCompatibilityResult ca ts mdat fib) =
    text "ApplicationCompatibilityResult" <+> parens (makeDoc ca)
      <+> parens (makeDoc ts) <+> parens (makeDoc mdat) <+> parens (makeDoc fib)
