{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, TemplateHaskell #-}

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
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger
import Language.TinyBang.Utils.TemplateHaskell.Transform

$(loggingFunctions)

-- |Performs substitutions on a constraint set to ensure that it is well-formed.
--  A constraint set is well-formed if none of the contours appearing within it
--  overlap.  This function takes a set of contours @cntrs@ which may overlap
--  with other contours in the constraint set; it then computes new contours
--  to use in the place of any contours which overlap with other contours.  The
--  input constraint set is assumed to be well-formed except in cases  regarding
--  the set of provided contours; if this is true, then the output constraint
--  set is well-formed.
contourReplaceVars :: Set Contour
                   -> SimpleConstraintDatabase
                   -> SimpleConstraintDatabase
contourReplaceVars cntrs db =
  -- Start by calculating every (unique) contour in the database
  let allContours =
        Set.fromList $ mapMaybe contourOfVar $ Set.toList $ findAllVars db in
  -- Next, determine if any of the input contours overlap with each other
  let cntrs' = Set.map (`mergeIfOverlap` Set.toList cntrs) cntrs in
  -- Next, calculate the overlap for each of the existing contours with these
  -- new ones
  let cntrs'' = Set.map (`mergeIfOverlap` Set.toList allContours) cntrs' in
  -- This new set of contours is disjoint and each contour in it entirely
  -- subsumes any contour in the constraint database that it overlaps.
  -- Therefore, any contour in the database which overlaps a contour in this new
  -- set should be replaced by the latter contour.  Do that now.
  _debugI
    (display $ align $
      text "cntrs:" <+> makeDoc cntrs </>
      text "allContours:" <+> makeDoc allContours </>
      text "cntrs':" <+> makeDoc cntrs' </>
      text "cntrs'':" <+> makeDoc cntrs'')
    $ transform (ReplaceVars cntrs'') db
  where
    mergeIfOverlap :: Contour -> [Contour] -> Contour
    mergeIfOverlap cntr others =
      foldl union cntr $
        filter (\cntr' -> overlap cntr cntr' && cntr /= cntr') others
  
data ReplaceVars = ReplaceVars (Set Contour)

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
                  , ''BuiltinOp
                  ])
$(defineCommonHomInstances ''ReplaceVars)

instance Transform ReplaceVars TVar where
  transform (ReplaceVars cntrs) (TVar x pcntr) =
    TVar x $ PossibleContour $ mightReplace <$> unPossibleContour pcntr
    where
      mightReplace cntr' =
        let replacements =
              mapMaybe (\cntr -> if cntr' `subsumedBy` cntr
                                    then Just cntr
                                    else Nothing) $
                Set.toList cntrs
        in
        case replacements of
          [] -> cntr' -- The new contours don't replace this one
          [cntr''] -> cntr'' -- We found a replacement contour
          _ -> error $ display $ text "Multiple replacements for contour" <+>
                        makeDoc cntr' <+> text "have been found:" <+>
                        makeDoc replacements
