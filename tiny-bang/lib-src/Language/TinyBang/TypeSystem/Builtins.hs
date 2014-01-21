{-# LANGUAGE ScopedTypeVariables #-}

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
    OpIntPlus -> builtinIntPlusConstraints
    OpIntMinus -> builtinIntMinusConstraints
    OpIntEq -> builtinIntEqConstraints
    OpIntLessEq -> builtinIntLessEqConstraints
    OpIntGreaterEq -> builtinIntGreaterEqConstraints
    OpSet -> builtinSetConstraints
    
builtinIntegerOperationConstraints ::
  forall db. (ConstraintDatabase db) => BuiltinOp -> db
builtinIntegerOperationConstraints =
  makeBuiltinScape
    (\a h -> CDb.singleton $ TPrimitive PrimInt <: a .: h)

builtinIntPlusConstraints :: (ConstraintDatabase db) => db
builtinIntPlusConstraints = builtinIntegerOperationConstraints OpIntPlus

builtinIntMinusConstraints :: (ConstraintDatabase db) => db
builtinIntMinusConstraints = builtinIntegerOperationConstraints OpIntMinus

builtinIntegerComparisonConstraints ::
  forall db. (ConstraintDatabase db) => BuiltinOp -> db
builtinIntegerComparisonConstraints bop =
  let a10 = derivVar $ getBuiltinEmptyOnionVar $ getBuiltinSpecifier bop in
  makeBuiltinScape
    (\a h -> CDb.fromList
                [ TEmptyOnion <: a10 .: h
                , TLabel (LabelName generated "True") (mktov a10) <: a .: h
                , TLabel (LabelName generated "False") (mktov a10) <: a .: h
                ])
    bop

builtinIntEqConstraints :: forall db. (ConstraintDatabase db) => db
builtinIntEqConstraints = builtinIntegerComparisonConstraints OpIntEq

builtinIntLessEqConstraints :: forall db. (ConstraintDatabase db) => db
builtinIntLessEqConstraints = builtinIntegerComparisonConstraints OpIntLessEq

builtinIntGreaterEqConstraints :: (ConstraintDatabase db) => db
builtinIntGreaterEqConstraints = builtinIntegerComparisonConstraints OpIntGreaterEq

builtinSetConstraints :: (ConstraintDatabase db) => db
builtinSetConstraints =
  -- FIXME: there is no backflow rule here
  makeBuiltinScape
    (\a h -> CDb.fromList
                [ TEmptyOnion <: a .: h
                ])
    OpSet

-- |A function which, given an output type and set of supporting constriants,
--  constructs an appropriate scape constraint for a builtin.
makeBuiltinScape :: (ConstraintDatabase db)
                 => (TVar -> ConstraintHistory db -> db) -> BuiltinOp -> db
makeBuiltinScape csF bop =
  let spec = getBuiltinSpecifier bop in
  let pat = Pattern generated $ getBuiltinPattern spec in
  let rvar = getBuiltinReturnVar spec in
  
  -- The pattern derivation should never raise an error
  let (tvPat, dbPat) =
        either (error . ("Error in builtin derivation of pattern: " ++) . show)
          id $ initialPatternDerivation pat in
          
  let tvExpr = derivVar rvar in
  let h = BuiltinTypeGeneration bop in
  let dbExpr = csF tvExpr h in
  
  let scapeType = TScape tvPat dbPat tvExpr dbExpr in
  let tvBuiltin = derivVar $ Var generated $ varname spec in
  CDb.singleton $ scapeType <: tvBuiltin .: h
