{-|
  This module provides a routine for determining the initial constraint database
  for an expression.
-}
module Language.TinyBang.TypeSystem.TypeInference.InitialDerivation
( InitialDerivationError(..)
, initialDerivation
) where

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintDatabase

-- |A datatype for errors which may occur during initial derivation.
data InitialDerivationError
  = NotClosed SomeVar

-- |Derives the initial constraint database for a given expression. 
initialDerivation :: (ConstraintDatabase db)
                  => Expr -> Either InitialDerivationError db
initialDerivation = undefined -- TODO
