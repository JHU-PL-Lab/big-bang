{-|
  This module defines the type system components for builtins.
-}
module Language.TinyBang.TypeSystem.Builtins
( builtinDb
) where

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Builtins
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.InitialDerivation
import Language.TinyBang.TypeSystem.Types

-- |A constraint set containing all of the types of the builtin functions.
builtinDb :: (ConstraintDatabase db) => db
builtinDb =
  foldl CDb.union CDb.empty $ map builtinConstraints $ enumFrom minBound

-- |A function which obtains the appropriate constraint set for a given builtin.
builtinConstraints :: (ConstraintDatabase db) => BuiltinOp -> db
builtinConstraints bop =
  case bop of
    Plus -> builtinPlusConstraints

builtinPlusConstraints :: (ConstraintDatabase db) => db
builtinPlusConstraints =
  makeBuiltinScape (TPrimitive PrimInt) CDb.empty Plus

-- |A function which, given an output type and set of supporting constriants,
--  constructs an appropriate scape constraint for a builtin.
makeBuiltinScape :: (ConstraintDatabase db)
                 => Type db -> db -> BuiltinOp -> db
makeBuiltinScape t cs bop =
  let spec = getBuiltinSpecifier bop in
  let pat = Pattern generated $ getBuiltinPattern spec in
  let rvar = getBuiltinReturnVar spec in
  
  -- The pattern derivation should never raise an error
  let (tvPat, dbPat) =
        either (error . ("Error in builtin derivation of pattern: " ++) . show)
          id $ initialPatternDerivation pat in
          
  let tvExpr = derivVar rvar in
  let h = BuiltinTypeGeneration bop in
  let dbExpr = CDb.singleton (t <: tvExpr .: h) `CDb.union` cs in
  
  let scapeType = TScape tvPat dbPat tvExpr dbExpr in
  let tvBuiltin = derivVar $ Var generated $ varname spec in
  CDb.singleton $ scapeType <: tvBuiltin .: h
