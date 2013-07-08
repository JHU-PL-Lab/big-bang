{-|
  Contains routines regarding variables which are used in the interpreter.
-}
module Language.PatBang.Interpreter.Variables
( freshen
, calculateBoundVariables
) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast
import Language.PatBang.Interpreter.Basis

-- |A routine to freshen every variable bound by the provided expression.
freshen :: Expr -> EvalM Expr
freshen e = do
  substs <- mapM makeSubst $ Set.toList $ calculateBoundVariables e
  return $ substitute (collect substs) e
  where
    makeSubst :: FlowVar -> EvalM Substitution
    makeSubst x = FlowSubstitution <$> freshFlowVar x <*> pure x

class HasVariableBindings a where
  calculateBoundVariables :: a -> Set FlowVar
  
instance HasVariableBindings Expr where
  calculateBoundVariables (Expr _ cls) =
    Set.unions $ map calculateBoundVariables cls
    
instance HasVariableBindings Clause where
  calculateBoundVariables cl = case cl of
    RedexDef _ x _ -> Set.singleton x
    ValueDef _ x _ -> Set.singleton x
