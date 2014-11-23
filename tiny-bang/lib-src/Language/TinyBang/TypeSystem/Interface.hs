{-# LANGUAGE Rank2Types #-}

{-|
  Gives a definition for a TinyBang type system implementation.  This interface
  makes use of the data types given in the simple type system implementation;
  all type systems are expected to be able to present results in that form.
-}
module Language.TinyBang.TypeSystem.Interface
( TypeSystem(..)
, TypecheckResult(..)
, TypecheckError(..)
, Inconsistency(..)
) where

import Data.Set (Set)

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Simple.Data

-- |A structure representing a TinyBang type system.
data TypeSystem
  = TypeSystem
      { typecheck :: Expr -> TypecheckResult
      }

-- |A structure representing the result of typechecking.
data TypecheckResult
  = TypecheckResult
      { allConstraints :: ConstraintSet
      , typeErrors :: Set TypecheckError
      }
