{-# LANGUAGE ScopedTypeVariables #-}

{-|
  This module is the top level of the type inference system.  It provides the
  top-level type inference routines for TinyBang.
-}
module Language.TinyBang.TypeSystem.TypeInference
( TypecheckingError(..)
, typecheck
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Closure
import Language.TinyBang.TypeSystem.Inconsistency
import Language.TinyBang.TypeSystem.InitialDerivation
import Language.TinyBang.TypeSystem.Types

-- |A data structure defining typechecking errors.
data TypecheckingError db
  = InitialDerivationFailed InitialDerivationError
  | ClosureFailed (ClosureError db)
  | ClosureInconsistent (Set Inconsistency) db 

-- |A function which performs top-level typechecking.  The caller must
--  provide an expression over which to perform typechecking.  The result
--  of this function will be a constraint database (if typechecking was
--  successful) or an error containing as much information as was obtained.
typecheck :: forall db. (ConstraintDatabase db)
          => Expr -> Either (TypecheckingError db) db
typecheck expr = do
  derivDb::db <- bailWith InitialDerivationFailed $ initialDerivation expr
  let startDb = instantiateContours initialContour derivDb
  closedDb <- bailWith ClosureFailed $ calculateClosure startDb
  let inconsistencies = determineInconsistencies closedDb
  if Set.null inconsistencies
    then Right closedDb
    else Left $ ClosureInconsistent inconsistencies closedDb
  where
    bailWith :: (a -> TypecheckingError db) -> Either a b
             -> Either (TypecheckingError db) b
    bailWith f = either (Left . f) Right
