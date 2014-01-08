{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

{-|
  This module defines a mechanism for replacing all variables in a set of
  constraints in a contour-unifying manner.  This is used to preserve the
  disjointedness of contours during simple constraint database closure.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.ReplaceVars
( contourReplaceVars
) where

import Control.Applicative
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Data
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.FindVars
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours as Cntr
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.TemplateHaskell.Transform

contourReplaceVars :: Set Contour
                   -> SimpleConstraintDatabase
                   -> SimpleConstraintDatabase
contourReplaceVars cntrs db =
  let allContours =
        Set.fromList $ mapMaybe contourOfVar $ Set.toList $ findAllVars db in
  let novelContours = cntrs Set.\\ allContours in
  if Set.null novelContours then db else
    let coveringContour = foldl1 Cntr.union $ Set.toList novelContours in
    let overlaps = Set.filter (overlap coveringContour) allContours in
    let cntr' = foldl Cntr.union coveringContour $ Set.toList overlaps in
    transform (ReplaceVars cntr') db
        
  
data ReplaceVars = ReplaceVars Contour

-- == Go go Gadget Template Haskell == -----------------------------------------

$(concat <$> mapM (defineHomInstance ''ReplaceVars)
                  [ ''SimpleConstraintDatabase
                  , ''Constraint
                  , ''Inconsistency
                  , ''Type
                  , ''TypeOrVar
                  , ''ConstraintHistory
                  , ''ClosureRuleInstance
                  ])
$(concat <$> mapM (defineTransformIdentityInstance ''ReplaceVars)
                  [ ''Origin
                  , ''LabelName
                  , ''PrimitiveType
                  , ''SourceElement
                  ])
$(defineCommonHomInstances ''ReplaceVars)

instance Transform ReplaceVars TVar where
  transform (ReplaceVars cntr) (TVar x pcntr) =
    TVar x $ PossibleContour $ mightReplace <$> unPossibleContour pcntr
    where
      mightReplace cntr' = if cntr' `subsumedBy` cntr then cntr else cntr'
