{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

{-|
  This module defines a routine for the polyinstantiation of variables by
  substitution.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Polyinst
( polyinst
) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.TemplateHaskell.Transform

polyinst :: (Ord db)
         => (db -> Set (Constraint db))
         -> (Set (Constraint db) -> db)
         -> Set TVar
         -> Contour
         -> db
         -> db
polyinst destr constr boundVars cntr db =
  constr $ transform (PolyInst constr destr boundVars cntr) (destr db)
  
data PolyInst db
  = PolyInst
      { polyInstConstr :: Set (Constraint db) -> db
      , polyInstDestr :: db -> Set (Constraint db)
      , polyInstBoundVars :: Set TVar
      , polyInstContour :: Contour
      }

$(concat <$> mapM (defineHomInstance ''PolyInst)
    [ ''Constraint
    , ''TypeOrVar
    ])
$(concat <$> mapM (defineTransformIdentityInstance ''PolyInst)
    [ ''ConstraintHistory
    ])
$(defineCommonHomInstances ''PolyInst)

instance Transform (PolyInst db) TVar where
  transform p a@(TVar x pcntr) =
    if a `Set.member` polyInstBoundVars p then a else
      case unPossibleContour pcntr of
        Just _ -> a
        Nothing ->
          TVar x $ PossibleContour $ Just $ polyInstContour p

instance (Ord db) => Transform (PolyInst db) (Type db) where
  transform p t = case t of
    TEmptyOnion -> TEmptyOnion
    TPrimitive _ -> t
    TLabel n tov -> TLabel n $ transform p tov
    TOnion tov1 tov2 -> TOnion (transform p tov1) (transform p tov2)
    TScape a' db' a db ->
      let patBound = topBound db' in
      let patTrans = p { polyInstBoundVars =
                          polyInstBoundVars p `Set.union` patBound } in
      let a'new = transform patTrans a' in
      let db'new =
            polyInstConstr p $ transform patTrans $ polyInstDestr p db' in
      let exprBound = topBound db in
      let exprTrans = p { polyInstBoundVars =
                          polyInstBoundVars p `Set.union` patBound `Set.union`
                            exprBound } in
      let anew = transform exprTrans a in
      let dbnew = polyInstConstr p $ transform exprTrans $ polyInstDestr p db in
      TScape a'new db'new anew dbnew
      where
        topBound db'' = Set.fromList $ map f $ Set.toList $ polyInstDestr p db''
          where
            f c = case c of
                    TypeConstraint _ _ a1 -> a1
                    IntermediateConstraint _ _ a2 -> a2
                    ApplicationConstraint _ _ _ a3 -> a3
