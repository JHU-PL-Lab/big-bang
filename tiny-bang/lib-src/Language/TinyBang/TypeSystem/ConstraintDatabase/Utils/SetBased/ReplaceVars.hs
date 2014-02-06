{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

{-|
  This module defines a mechanism for replacing all variables in a set of
  constraints in a contour-unifying manner.  This is used to preserve the
  disjointedness of contours during simple constraint database closure.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.ReplaceVars
( replaceVarsByContours

, ReplaceVars(..)
, createReplaceVarsInstances
) where

import Control.Applicative
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours as Cntr
import Language.TinyBang.TypeSystem.Types as TBT
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger
import Language.TinyBang.Utils.TemplateHaskell.Transform

$(loggingFunctions)

-- TODO: rename module to "ReplaceContours"

-- |Defines a function which performs variable replacements using a given set of
--  contours.  Any variable with a contour entirely subsumed by one in the
--  provided set will have its contour replaced by that subsuming contour.  The
--  provided set of contours is assumed to be disjoint.
replaceVarsByContours :: (Transform ReplaceVars a)
                     => Set Contour -> a -> a
replaceVarsByContours cntrs = transform (ReplaceVars cntrs)
  
data ReplaceVars = ReplaceVars (Set Contour)

-- == Go go Gadget Template Haskell == -----------------------------------------

$(concat <$> mapM (defineHomInstance ''ReplaceVars)
                  [ ''Constraint
                  , ''Inconsistency
                  , ''TBT.Type
                  , ''TypeOrVar
                  , ''ConstraintHistory
                  , ''ClosureRuleInstance
                  , ''TVar
                  , ''PossibleContour
                  ])
$(concat <$> mapM (defineTransformIdentityInstance ''ReplaceVars)
                  [ ''Origin
                  , ''LabelName
                  , ''PrimitiveType
                  , ''SourceElement
                  , ''BuiltinOp
                  , ''Var
                  ])
$(defineCommonHomInstances ''ReplaceVars)

instance Transform ReplaceVars Contour where
  transform (ReplaceVars cntrs) cntr' =
    let replacements =
          mapMaybe (\cntr -> if cntr' `subsumedBy` cntr
                                then Just cntr
                                else Nothing) $
            Set.toList cntrs
    in
    -- TODO: only do the multiplicity check if we're in debug mode (which
    --       hasn't been implemented yet!)
    case replacements of
      [] -> cntr' -- The new contours don't replace this one
      [cntr''] -> cntr'' -- We found a replacement contour
      _ -> error $ display $ text "Multiple replacements for contour" <+>
                    makeDoc cntr' <+> text "have been found:" <+>
                    makeDoc replacements

-- |Defines an instance of variable replacement for a set-based constraint
--  database.
createReplaceVarsInstances :: Name -> Q [Dec]
createReplaceVarsInstances = defineHomInstance ''ReplaceVars
