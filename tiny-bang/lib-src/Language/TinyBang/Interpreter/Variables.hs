{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, FlexibleInstances, TupleSections #-}

{-|
  Contains routines regarding variables which are used in the interpreter.
-}
module Language.TinyBang.Interpreter.Variables
( freshen
, findAllVariables
, findBoundVariables
) where

import Control.Applicative
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Interpreter.Substitution
import Language.TinyBang.Utils.TemplateHaskell.Reduce

-- PERF: rewrite in terms of TransformM for efficiency

-- |A routine to freshen every variable bound by the provided expression.
freshen :: Expr -> EvalM Expr
freshen e = do
  let vars = Set.toList $ findBoundVariables e
  subst <- Map.fromList <$> mapM freshVarPair vars
  return $ substitute subst e
  where
    freshVarPair v = (v,) <$> freshVar v
    
-- |A routine to locate all variables located within the provided expression.
findAllVariables :: Expr -> Set Var
findAllVariables = reduce FindAllVariables

-- |A routine to locate all variables bound by the provided expression.
findBoundVariables :: Expr -> Set Var
findBoundVariables = reduce FindBoundVariables

data FindAllVariables = FindAllVariables
data FindBoundVariables = FindBoundVariables

--------------------------------------------------------------------------------

$(concat <$> mapM (defineCatInstance [t|Set Var|] ''FindAllVariables)
                  [ ''Expr
                  , ''Clause
                  , ''Redex
                  , ''Value
                  , ''Pattern
                  , ''PatternClause
                  , ''PatternValue
                  ])
$(concat <$> mapM (defineReduceEmptyInstance [t|Set Var|] ''FindAllVariables)
                  [ ''Origin
                  , ''PrimitiveValue
                  , ''LabelName
                  , ''PrimitiveType
                  , ''BuiltinOp
                  ])
$(defineCommonCatInstances [t|Set Var|] ''FindAllVariables)

instance Reduce FindAllVariables Var (Set Var) where
  reduce FindAllVariables = Set.singleton

--------------------------------------------------------------------------------

$(concat <$> mapM (defineCatInstance [t|Set Var|] ''FindBoundVariables)
                  [ ''Expr
                  , ''Pattern
                  ])
$(concat <$> mapM (defineReduceEmptyInstance [t|Set Var|] ''FindBoundVariables)
                  [ ''Origin
                  ])
$(defineCommonCatInstances [t|Set Var|] ''FindBoundVariables)

instance Reduce FindBoundVariables Clause (Set Var) where
  reduce FindBoundVariables (Clause _ x _) = Set.singleton x
