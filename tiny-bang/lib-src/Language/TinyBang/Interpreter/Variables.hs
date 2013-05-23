{-|
  Contains routines regarding variables which are used in the interpreter.
-}
module Language.TinyBang.Interpreter.Variables
( freshen
, calculateBoundVariables
) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis

-- |A routine to freshen every variable bound by the provided expression.
freshen :: Expr -> EvalM Expr
freshen e = do
  substs <- mapM makeSubst $ Set.toList $ calculateBoundVariables e
  return $ substitute (collect substs) e
  where
    makeSubst :: AnyVar -> EvalM Substitution
    makeSubst v = case v of
      SomeFlowVar x -> FlowSubstitution <$> freshFlowVar x <*> pure x
      SomeCellVar y -> CellSubstitution <$> freshCellVar y <*> pure y

class HasVariableBindings a where
  calculateBoundVariables :: a -> Set AnyVar
  
instance HasVariableBindings Expr where
  calculateBoundVariables (Expr _ cls) =
    Set.unions $ map calculateBoundVariables cls
    
instance HasVariableBindings Clause where
  calculateBoundVariables cl = case cl of
    RedexDef _ x _ -> Set.singleton $ someVar x
    CellSet _ _ _ -> Set.empty
    CellGet _ x _ -> Set.singleton $ someVar x
    Throws _ x _ -> Set.singleton $ someVar x
    Evaluated ecl -> calculateBoundVariables ecl

instance HasVariableBindings EvaluatedClause where
  calculateBoundVariables ecl = case ecl of
    ValueDef _ x _ -> Set.singleton $ someVar x
    CellDef _ _ y _ -> Set.singleton $ someVar y
    Flow _ _ _ _ -> Set.empty
