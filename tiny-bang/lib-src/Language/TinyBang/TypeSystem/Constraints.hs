{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Language.TinyBang.TypeSystem.Constraints
( Constraint(..)
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
  deriving (Show)

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
    where
      subdoc = text "<:"

-- * Template Haskell instances
$(deriveEqSkipFirst ''Constraint)
$(deriveOrdSkipFirst ''Constraint)
