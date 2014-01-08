{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

{-|
  This module defines a mechanism for finding all variables in a set of
  constraints.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.FindVars
( findAllVars
) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple.Data
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.TemplateHaskell.Reduce

findAllVars :: (Reduce FindAllVars a (Set TVar)) => a -> Set TVar
findAllVars = reduce FindAllVars

data FindAllVars = FindAllVars

-- == Go go Gadget Template Haskell == -----------------------------------------

$(concat <$> mapM (defineCatInstance [t|Set TVar|] ''FindAllVars)
                  [ ''SimpleConstraintDatabase
                  , ''Constraint
                  , ''Inconsistency
                  , ''Type
                  , ''TypeOrVar
                  , ''ConstraintHistory
                  , ''ClosureRuleInstance
                  ])
$(concat <$> mapM (defineReduceEmptyInstance [t|Set TVar|] ''FindAllVars)
                  [ ''Origin
                  , ''LabelName
                  , ''PrimitiveType
                  , ''SourceElement
                  ])
$(defineCommonCatInstances [t|Set TVar|] ''FindAllVars)

instance Reduce FindAllVars TVar (Set TVar) where
  reduce FindAllVars = Set.singleton
