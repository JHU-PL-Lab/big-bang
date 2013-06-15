{-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving, ScopedTypeVariables #-}

module Language.TinyBang.TypeSystem.ConstraintHistory
( ConstraintHistory(..)
, SourceElement(..)
, ClosureRule(..)
, ProjectionResult(..)
, ProjectionResultForm(..)
, AnyProjectionResultForm(..)
, ApplicationCompatibilityResult(..)
) where

import Data.Monoid

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
      (ProjectionResult db)
        -- ^ The proof of integer projection for the left.
      (ProjectionResult db)
        -- ^ The proof of integer projection for the right.
  | IntegerCalculationRule
      OperationConstraint -- ^ The triggering constraint.
      (ProjectionResult db)
        -- ^ The proof of integer projection for the left.
      (ProjectionResult db)
        -- ^ The proof of integer projection for the right.
  | EqualityRule
      OperationConstraint -- ^ The triggering constraint.
  | ApplicationRule
      ApplicationConstraint -- ^ The triggering constraint.
      (ProjectionResult db) -- ^ The projection of scapes
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
      (ProjectionResult db) -- ^ The projection of scapes
      (ApplicationCompatibilityResult db) -- ^ Argument compatibility      
  | ExceptionCatchRule
      ApplicationConstraint -- ^ The triggering constraint.
      ExceptionConstraint -- ^ The exception constraint describing the argument
      (ProjectionResult db) -- ^ The projection of scapes
      (ApplicationCompatibilityResult db) -- ^ Argument compatibility
      Contour -- ^ The contour generated for the application
  deriving (Eq, Ord, Show)
  
-- |A data structure describing the approximate result of a projection.
data ProjectionResultForm db (tag :: A.ProjectorTag) where
  ProjectionResultPrimForm :: Bool
                           -> Fibration db
                           -> ProjectionResultForm db A.ProjPrimTag
  ProjectionResultLabelForm :: [CellTVar]
                            -> Fibration db
                            -> ProjectionResultForm db A.ProjLabelTag
  ProjectionResultFunForm :: [Type db]
                          -> Fibration db
                          -> ProjectionResultForm db A.ProjFunTag
deriving instance (Eq db) => Eq (ProjectionResultForm db tag)
deriving instance (Show db) => Show (ProjectionResultForm db tag)
instance (Display db, DocumentContainer db)
      => Display (ProjectionResultForm db tag) where
  makeDoc form = case form of
    ProjectionResultPrimForm a b ->
      text "ProjectionResultPrimForm" <+> makeDoc a <+> makeDoc b
    ProjectionResultLabelForm a b ->
      text "ProjectionResultLabelForm" <+> makeDoc a <+> makeDoc b
    ProjectionResultFunForm a b ->
      text "ProjectionResultFunForm" <+> makeDoc a <+> makeDoc b

data AnyProjectionResultForm db =
  forall (tag :: A.ProjectorTag).
    SomeProjectionResultForm (ProjectionResultForm db tag)
instance (Ord db) => Eq (AnyProjectionResultForm db) where
  a == b = compare a b == EQ
instance (Ord db) => Ord (AnyProjectionResultForm db) where
  compare a b = case (a,b) of
    (   SomeProjectionResultForm (ProjectionResultPrimForm x f)
      , SomeProjectionResultForm (ProjectionResultPrimForm x' f')) ->
      mconcat [compare x x', compare f f']
    (   SomeProjectionResultForm (ProjectionResultPrimForm _ _)
      , SomeProjectionResultForm (ProjectionResultLabelForm _ _)) -> LT
    (   SomeProjectionResultForm (ProjectionResultPrimForm _ _)
      , SomeProjectionResultForm (ProjectionResultFunForm _ _)) -> LT
    (   SomeProjectionResultForm (ProjectionResultLabelForm _ _)
      , SomeProjectionResultForm (ProjectionResultPrimForm _ _)) -> GT
    (   SomeProjectionResultForm (ProjectionResultLabelForm bs f)
      , SomeProjectionResultForm (ProjectionResultLabelForm bs' f')) ->
      mconcat [compare bs bs', compare f f']
    (   SomeProjectionResultForm (ProjectionResultLabelForm _ _)
      , SomeProjectionResultForm (ProjectionResultFunForm _ _)) -> LT
    (   SomeProjectionResultForm (ProjectionResultFunForm _ _)
      , SomeProjectionResultForm (ProjectionResultPrimForm _ _)) -> GT
    (   SomeProjectionResultForm (ProjectionResultFunForm _ _)
      , SomeProjectionResultForm (ProjectionResultLabelForm _ _)) -> GT
    (   SomeProjectionResultForm (ProjectionResultFunForm ts f)
      , SomeProjectionResultForm (ProjectionResultFunForm ts' f')) ->
      mconcat [compare ts ts', compare f f']
  
data ProjectionResult db where
  ProjectionResult :: forall db' (tag :: A.ProjectorTag).
                      A.Projector tag
                   -> FlowTVar
                   -> ProjectionResultForm db' tag
                   -> ProjectionResult db'

instance (Ord db) => Eq (ProjectionResult db) where
  a == b = compare a b == EQ 
instance (Ord db) => Ord (ProjectionResult db) where
  compare x y = case (x,y) of
    (ProjectionResult proj a form, ProjectionResult proj' a' form') ->
      mconcat [ compare (A.SomeProjector proj) (A.SomeProjector proj')
              , compare a a'
              , compare (SomeProjectionResultForm form)
                        (SomeProjectionResultForm form')
              ]
instance (Show db) => Show (ProjectionResult db) where
  show (ProjectionResult proj a form) =
    "ProjectionResult " ++ show (A.SomeProjector proj) ++ " " ++ show a ++ " " ++ show form 

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
      => Display (ProjectionResult db) where
  makeDoc (ProjectionResult proj a result) =
    text "ProjectionResult" <+> parens (makeDoc proj)
      <+> parens (makeDoc a) <+> parens (makeDoc result)

instance (Display db, DocumentContainer db)
      => Display (ApplicationCompatibilityResult db) where
  makeDoc (ApplicationCompatibilityResult ca ts mdat fib) =
    text "ApplicationCompatibilityResult" <+> parens (makeDoc ca)
      <+> parens (makeDoc ts) <+> parens (makeDoc mdat) <+> parens (makeDoc fib)
