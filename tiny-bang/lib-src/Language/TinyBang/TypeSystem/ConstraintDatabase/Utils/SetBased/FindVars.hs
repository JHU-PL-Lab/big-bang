{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, ConstraintKinds #-}

{-|
  This module defines a mechanism for finding all variables in a set of
  constraints.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase.Utils.SetBased.FindVars
( findAllVars
, FindAllVarsable

, FindAllVars(..)
, createFindAllVarsInstances
) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH as TH

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Types as TBT
import Language.TinyBang.Utils.TemplateHaskell.Reduce

type FindAllVarsable a = (Reduce FindAllVars a (Set TVar))

findAllVars :: (Reduce FindAllVars a (Set TVar)) => a -> Set TVar
findAllVars = reduce FindAllVars

data FindAllVars = FindAllVars

-- == Go go Gadget Template Haskell == -----------------------------------------

$(concat <$> mapM (defineCatInstance [t|Set TVar|] ''FindAllVars)
                  [ ''Constraint
                  , ''Inconsistency
                  , ''TBT.Type
                  , ''TypeOrVar
                  , ''ConstraintHistory
                  , ''ClosureRuleInstance
                  ])
$(concat <$> mapM (defineReduceEmptyInstance [t|Set TVar|] ''FindAllVars)
                  [ ''Origin
                  , ''LabelName
                  , ''PrimitiveType
                  , ''SourceElement
                  , ''BuiltinOp
                  ])
$(defineCommonCatInstances [t|Set TVar|] ''FindAllVars)

instance Reduce FindAllVars TVar (Set TVar) where
  reduce FindAllVars = Set.singleton

createFindAllVarsInstances :: Name -> Q [Dec]
createFindAllVarsInstances = defineCatInstance [t|Set TVar|] ''FindAllVars
