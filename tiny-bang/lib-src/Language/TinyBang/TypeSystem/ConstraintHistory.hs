module Language.TinyBang.TypeSystem.ConstraintHistory
( ConstraintHistory(..)
, SourceElement(..)
, ClosureRuleInstance(..)
) where

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Types

data ConstraintHistory db
  = DerivedFromSource SourceElement
      -- ^Describes a constraint generated during initial derivation.
  | CompatibilityWiring (TypeOrVar db) TVar TVar
      -- ^Describes a constraint generated from a compatibility check.  The
      --  arguments are the scape, argument, and call site which inspired the
      --  compatibility check, respectively.
  | DerivedFromClosure (ClosureRuleInstance db)
      -- ^Describe a constraint generated from a constraint closure rule.
  deriving (Eq, Ord, Show)

data SourceElement
  = ClauseElement Clause
  | PatternElement PatternClause
  deriving (Eq, Ord, Show)
  
data ClosureRuleInstance db
  = TransitivityRule (Type db) TVar TVar
      -- ^Describes the transitivity closure rule.  The arguments are the
      --  t, a1, and a2 from {t <: a1, a1 <: a2}.
  | ApplicationRule TVar TVar TVar (Maybe (Type db)) (TypeOrVar db)
      {- ^Describes an instance of the application rule.  The arguments are:
            * The scape variable
            * The argument variable
            * The call site variable
            * The type of the scape which was used (if available)
            * The slice of the argument which was used
      -}
  deriving (Eq, Ord, Show)

