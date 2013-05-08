{-|
  A module for describing constraint history.  Each constraint is entered into
  a constraint database with a history describing how it was concluded.
-}
module Language.TinyBang.TypeSystem.ConstraintHistory
( ConstraintHistory(..)
, SourceElement(..)
) where

import Language.TinyBang.Ast

data ConstraintHistory db
  = DerivedFromSource SourceElement
  deriving (Eq, Ord, Show)
-- TODO

data SourceElement
  = ClauseElement Clause
  | PatternElement Pattern
  | QualifierElement CellQualifier
  deriving (Eq, Ord, Show)

instance HasOrigin SourceElement where
  originOf x = case x of
    ClauseElement cl -> originOf cl
    PatternElement pat -> originOf pat
    QualifierElement q -> originOf q
