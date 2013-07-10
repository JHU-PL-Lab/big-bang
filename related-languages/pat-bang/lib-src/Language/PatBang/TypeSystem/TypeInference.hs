{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

{-|
  This module is the top level of the type inference system.  It provides the
  top-level type inference routines for PatBang.
-}
module Language.PatBang.TypeSystem.TypeInference
( TypecheckingError(..)
, typecheck
) where

import qualified Data.Set as Set

import Language.PatBang.Ast
import Language.PatBang.Display
import Language.PatBang.Logging  
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.Contours
import Language.PatBang.TypeSystem.Closure
import Language.PatBang.TypeSystem.Inconsistency
import Language.PatBang.TypeSystem.InitialDerivation

$(loggingFunctions)

-- |A data structure defining typechecking errors.
data TypecheckingError db
  = InitialDerivationFailed InitialDerivationError
  | ClosureFailed (ClosureError db)
  | InconsistencyFailed (InconsistencyError db)
  | ClosureInconsistent [Inconsistency db] db
  deriving (Eq, Ord, Show)

instance (ConstraintDatabase db, Display db)
      => Display (TypecheckingError db) where
  makeDoc err = case err of
    InitialDerivationFailed err' ->
      text "InitialDerivationFailed" <+> makeDoc err'
    ClosureFailed err' ->
      text "ClosureFailed" <+> makeDoc err'
    InconsistencyFailed err' ->
      text "InconsistencyFailed" <+> makeDoc err'
    ClosureInconsistent incons db ->
      text "ClosureInconsistent" <+> nest 2
        (linebreak <> makeDoc incons <$$> makeDoc db)

-- |A function which performs top-level typechecking.  The caller must
--  provide an expression over which to perform typechecking.  The result
--  of this function will be a constraint database (if typechecking was
--  successful) or an error containing as much information as was obtained.
typecheck :: forall db. (ConstraintDatabase db, Display db, Ord db)
          => Expr -> Either (TypecheckingError db) db
typecheck expr = do
  _debug $ "Typechecking expression: " ++ display expr
  derivDb::db <- bailWith "Initial derivation" InitialDerivationFailed $
                    initialDerivation expr
  _debug $ "Initial derivation produced: " ++ display derivDb
  let startDb = instantiateContours Set.empty initialContour derivDb
  _debug $ "After initial instantiation: " ++ display startDb
  closedDb <- bailWith "Closure" ClosureFailed $ calculateClosure startDb
  _debug $ "After closure: " ++ display closedDb
  inconsistencies <- bailWith "Inconsistency check" InconsistencyFailed $
                        determineInconsistencies closedDb
  if null inconsistencies
    then _debugI "Typechecking complete." $ Right closedDb
    else Left $ ClosureInconsistent inconsistencies closedDb
  where
    bailWith :: (Display a)
             => String -> (a -> TypecheckingError db) -> Either a b
             -> Either (TypecheckingError db) b
    bailWith msg f = either (Left . f . withMsg) Right
      where withMsg x = _debugI (msg ++ " failed: " ++ display x) x
          
