{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

{-|
  This module defines a routine for the polyinstantiation of variables by
  substitution.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.Polyinst
( polyinst

, PolyInst
, createPolyinstInstances
) where

import Control.Applicative
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface as CDb
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types as TBT
import Language.TinyBang.Utils.TemplateHaskell.Transform

polyinst :: (Transform PolyInst a)
         => Contour -> a -> a
polyinst cntr = transform $ PolyInst Set.empty cntr
  
data PolyInst
  = PolyInst
      { polyInstBoundVars :: Set TVar
      , polyInstContour :: Contour
      }

$(concat <$> mapM (defineHomInstance ''PolyInst)
    [ ''Constraint
    , ''Inconsistency
    , ''TypeOrVar
    ])
$(concat <$> mapM (defineTransformIdentityInstance ''PolyInst)
    [ ''ConstraintHistory
    , ''BuiltinOp
    ])
$(defineCommonHomInstances ''PolyInst)

instance Transform PolyInst TVar where
  transform p a@(TVar x pcntr) =
    if a `Set.member` polyInstBoundVars p then a else
      case unPossibleContour pcntr of
        Just _ -> a
        Nothing ->
          TVar x $ PossibleContour $ Just $ polyInstContour p

instance (ConstraintDatabase db, Transform PolyInst db)
      => Transform PolyInst (TBT.Type db) where
  transform p t = case t of
    TEmptyOnion -> TEmptyOnion
    TPrimitive _ -> t
    TLabel n tov -> TLabel n $ transform p tov
    TRef a -> TRef $ transform p a
    TOnion tov1 tov2 -> TOnion (transform p tov1) (transform p tov2)
    TScape a' db' a db ->
      let patBound = topBound db' in
      let patTrans = p { polyInstBoundVars =
                          polyInstBoundVars p `Set.union` patBound } in
      let a'new = transform patTrans a' in
      let db'new = transform patTrans db' in
      let exprBound = topBound db in
      let exprTrans = p { polyInstBoundVars =
                          polyInstBoundVars p `Set.union` patBound `Set.union`
                            exprBound } in
      let anew = transform exprTrans a in
      let dbnew = transform exprTrans db in
      TScape a'new db'new anew dbnew
      where
        topBound db'' = Set.fromList $ mapMaybe f $ Set.toList $
                          query db'' QueryAllConstraints
          where
            f c = case c of
                    TypeConstraint _ _ a1 -> Just a1
                    IntermediateConstraint _ _ a2 -> Just a2
                    ApplicationConstraint _ _ _ a3 -> Just a3
                    BuiltinConstraint _ _ _ a0 -> Just a0
                    InconsistencyConstraint _ _ -> Nothing

createPolyinstInstances :: Name -> Q [Dec]
createPolyinstInstances = defineHomInstance ''PolyInst
