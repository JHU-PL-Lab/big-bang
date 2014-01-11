{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

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
import Language.TinyBang.TypeSystem.Builtins
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Closure
import Language.TinyBang.TypeSystem.InitialDerivation
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger  

$(loggingFunctions)

-- |A data structure defining typechecking errors.
data TypecheckingError db
  = InitialDerivationFailed InitialDerivationError
  | ClosureInconsistent (Set (Inconsistency db)) db
  deriving (Eq, Ord, Show)

instance (ConstraintDatabase db, Display db)
      => Display (TypecheckingError db) where
  makeDoc err = case err of
    InitialDerivationFailed err' ->
      text "InitialDerivationFailed" <+> makeDoc err'
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
  derivDb::db <-
    bracketLogM _debugI
      ("Typechecking expression: " ++ display expr)
      (\db -> "Initial derivation produced: " ++ display db) $
        bailWith "Initial derivation" InitialDerivationFailed $
          initialDerivation expr
  let startDb = polyinstantiate initialContour $ CDb.union derivDb builtinDb
  let closedDb =
        bracketLog _debugI
          ("Performing constraint closure on " ++ display startDb)
          (\db -> "Completed constraint closure: " ++ display db) $
            calculateClosure startDb
  let inconsistencies =
        bracketLog _debugI
          ("Checking for inconsistencies in " ++ display closedDb)
          (\incon -> "Inconsistencies follow: " ++ display incon) $
            query closedDb QueryAllInconsistencies
  if Set.null inconsistencies
    then _debugI "Typechecking complete." $ Right closedDb
    else Left $ ClosureInconsistent inconsistencies closedDb
  where
    bailWith :: (Display a)
             => String -> (a -> TypecheckingError db) -> Either a b
             -> Either (TypecheckingError db) b
    bailWith msg f = either (Left . f . withMsg) Right
      where withMsg x = _debugI (msg ++ " failed: " ++ display x) x
