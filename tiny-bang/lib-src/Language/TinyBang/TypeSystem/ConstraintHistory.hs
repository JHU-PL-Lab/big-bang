{-|
  A module for describing constraint history.  Each constraint is entered into
  a constraint database with a history describing how it was concluded.
-}
module Language.TinyBang.TypeSystem.ConstraintHistory
( ConstraintHistory(..)
) where

import Language.TinyBang.Syntax.Location

data ConstraintHistory db
  = SourceHistory SourceRegion
  deriving (Eq, Ord, Show)
-- TODO