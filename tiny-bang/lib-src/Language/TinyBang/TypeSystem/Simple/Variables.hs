{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-|
  This module defines some homomorphisms and reductions over various type
  system data types, specifically relating to the treatment of type variables.
-}
module Language.TinyBang.TypeSystem.Simple.Variables
( substituteVars
, findTVarsDefinedBy
) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Simple.Data
import Language.TinyBang.Utils.TemplateHaskell.Reduce
import Language.TinyBang.Utils.TemplateHaskell.Transform

-- Public functions

substituteVars :: (Transform TVarSubstitution a) => (TVar -> TVar) -> a -> a
substituteVars = transform . TVarSubstitution

findTVarsDefinedBy :: (Reduce TVarsDefinedBy a (Set TVar)) => a -> Set TVar
findTVarsDefinedBy = reduce TVarsDefinedBy

-- Operation data types (appearing at the top to accommodate TH parsing)

data TVarSubstitution = TVarSubstitution (TVar -> TVar)
data TVarsDefinedBy = TVarsDefinedBy

-- Variable substitutions

$(concat <$> mapM (defineHomInstance ''TVarSubstitution)
                [ ''ConstraintSet
                , ''Constraint
                , ''FilteredType
                , ''PatternTypeSet
                , ''Type
                ]
 )
 
$(concat <$> mapM (defineTransformIdentityInstance ''TVarSubstitution)
                [ ''PatternType
                , ''LabelName
                , ''PrimitiveType
                ]
 )

$(defineCommonHomInstances ''TVarSubstitution)

instance Transform TVarSubstitution TVar where
  transform (TVarSubstitution f) = f
  
-- Variables defined by a constraint set

$(concat <$> mapM (defineCatInstance [t|Set TVar|] ''TVarsDefinedBy)
                    [ ''ConstraintSet
                    ]
 )

$(defineCommonCatInstances [t|Set TVar|] ''TVarsDefinedBy)

instance Reduce TVarsDefinedBy Constraint (Set TVar) where
  reduce TVarsDefinedBy c = case c of
    LowerBoundConstraint _ a -> Set.singleton a
    IntermediateConstraint _ a -> Set.singleton a
    ApplicationConstraint _ _ a -> Set.singleton a
