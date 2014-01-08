{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Language.TinyBang.TypeSystem.Constraints
( Constraint(..)
, Inconsistency(..)
, (<:)
, (.:)
) where

import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.TemplateHaskell.Deriving

-- |Represents TinyBang type constraints.  Parametric in the type of database
--  which appears in the scape types within these constraints.
data Constraint db
  = TypeConstraint (ConstraintHistory db) (Type db) TVar
  | IntermediateConstraint (ConstraintHistory db) TVar TVar
  | ApplicationConstraint (ConstraintHistory db) TVar TVar TVar
  | InconsistencyConstraint (ConstraintHistory db) (Inconsistency db)
  deriving (Show)
  
-- * Inconsistencies

data Inconsistency db
  = ApplicationFailure
      TVar TVar TVar (TypeOrVar db)
      {- ^Describes a failure of application.  The arguments are
            * The scape variable
            * The argument variable
            * The call site variable
            * The slice of the argument which was used
      -}
  deriving (Eq, Ord, Show)

instance (Display db) => Display (Inconsistency db) where
  makeDoc incon = case incon of
    ApplicationFailure sa aa csa as ->
      text "ApplicationFailure" <+> parens (makeDoc sa) <+>
        parens (makeDoc aa) <+> parens (makeDoc csa) <+> parens (makeDoc as)

-- * Convenience constructors

newtype ConstraintWithoutHistory db =
  ConstraintWithoutHistory (ConstraintHistory db -> Constraint db)

class ConstraintConstructable a b db where
  (<:) :: a -> b -> ConstraintWithoutHistory db

instance ConstraintConstructable (Type db) TVar db where
  a <: b = ConstraintWithoutHistory $ \h -> TypeConstraint h a b

instance ConstraintConstructable TVar TVar db where
  a <: b = ConstraintWithoutHistory $ \h -> IntermediateConstraint h a b

instance ConstraintConstructable (TVar, TVar) TVar db where
  (a,b) <: c = ConstraintWithoutHistory $ \h -> ApplicationConstraint h a b c
  
infix 7 <:

(.:) :: ConstraintWithoutHistory db -> ConstraintHistory db -> Constraint db
ConstraintWithoutHistory f .: h = f h
infix 6 .:

-- * Display instances

instance (Display db) => Display (Constraint db) where
  makeDoc c = case c of
    TypeConstraint _ t a ->
      makeDoc t <+> subdoc <+> makeDoc a
    IntermediateConstraint _ a a' ->
      makeDoc a <+> subdoc <+> makeDoc a'
    ApplicationConstraint _ a a' a'' ->
      makeDoc a <+> makeDoc a' <+> subdoc <+> makeDoc a''
    InconsistencyConstraint _ i ->
      text "Inconsistent" <+> parens (makeDoc i)
    where
      subdoc = text "<:"

-- * Template Haskell instances
$(deriveEqSkipFirst ''Constraint)
$(deriveOrdSkipFirst ''Constraint)
