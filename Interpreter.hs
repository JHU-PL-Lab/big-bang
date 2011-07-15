{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |A module defining a Big Bang interpreter.
-}
module Interpreter
( evalTop
, eval
, EvalError
, ErrorOrSuccess
, EvalM
) where

import Control.Monad.Error -- TODO: pare down the imports from this module
import Data.List (foldl')
import Data.Maybe (catMaybes)

import Ast (Branches, Chi(..), Expr(..))
import qualified Types as T
import UtilTypes (Ident, ident, unIdent, LabelName, labelName, unLabelName)

-- TODO: remove
import Debug.Trace

-- |An error type for evaluation failures.
data EvalError =
      ApplNotFunction Expr Expr
    | DynamicTypeError String -- TODO: figure out what goes here
    | NotClosed Ident
    | UnmatchedCase Expr Branches
    deriving (Show)

data ErrorOrSuccess a b = VLeft a | VRight b
    deriving (Show)

instance Monad (ErrorOrSuccess a) where
    VLeft a >>= f = VLeft a
    VRight a >>= f = f a
    return = VRight

instance MonadError EvalError (ErrorOrSuccess EvalError) where
    throwError = VLeft
    catchError (VLeft e) handler = handler e
    catchError (VRight v) _ = VRight v

type EvalM = ErrorOrSuccess EvalError Expr

------------------------------------------------------------------------------
-- *Evaluation Functions
-- $EvaluationFunctions
--
-- Definitions for functions related to expression evaluation.

-- |Performs top-level evaluation of a Big Bang expression.  This evaluation
--  routine binds built-in functions (like "plus") to the appropriate
--  expressions.
evalTop :: Expr -> EvalM
evalTop e = do
    eval $ foldl' applyBuiltin e builtins
    where applyBuiltin e (name, ast) =
            subst ast (ident name) e
          ix = ident "x"
          iy = ident "y"
          vx = Var ix
          vy = Var iy
          builtins = [
                ("plus", Func ix $ Func iy $ Plus vx vy)
              , ("minus", Func ix $ Func iy $ Minus vx vy)
              , ("equal", Func ix $ Func iy $ Equal vx vy)
              ]


-- |Evaluates a Big Bang expression.
eval :: Expr -> EvalM

eval (Var i) = throwError $ NotClosed i

eval (Label n e) = do
    e' <- eval e
    return $ Label n e'

eval (Onion e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    return $ Onion e1' e2'

eval (Func i e) = return $ Func i e

eval (Appl e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    case e1' of
        Func ident body -> eval $ subst e2' ident body
        _ -> throwError $ ApplNotFunction e1' e2'

eval (PrimInt i) = return $ PrimInt i

eval (PrimString s) = return $ PrimString s

eval PrimUnit = return $ PrimUnit

-- TODO: this is dynamic case, not static case!
eval (Case e branches) = do
    e' <- eval e
    let answers = catMaybes $ map (evalLabel e') branches
    case answers of
        [] -> throwError $ UnmatchedCase e' branches
        answer:_ -> eval answer
    where evalLabel e' (chi,branchExpr) =
              case (chi,e') of
                  (ChiPrim T.PrimInt, PrimInt _) -> Just branchExpr
                  (ChiPrim T.PrimString, PrimString _) -> Just branchExpr
                  (ChiPrim T.PrimUnit, PrimUnit) -> Just branchExpr
                  (ChiLabel name ident, Label name' lblExpr) ->
                      if name == name' then Just $ subst lblExpr ident branchExpr
                                       else Nothing
                  (ChiOnion ident1 ident2, Onion expr1 expr2) ->
                      Just $ subst expr1 ident1 $ subst expr2 ident2 branchExpr
                  (ChiFun, Func i e) -> Just branchExpr
                  _ -> Nothing

eval (Plus e1 e2) =
    evalIntBinop e1 e2 $ \x y -> PrimInt $ x + y

eval (Minus e1 e2) =
    evalIntBinop e1 e2 $ \x y -> PrimInt $ x - y

eval (Equal e1 e2) =
    evalIntBinop e1 e2 $ \x y ->
            Label (labelName (if x == y then "True" else "False")) PrimUnit

evalIntBinop :: Expr -> Expr -> (Integer -> Integer -> Expr) -> EvalM
evalIntBinop e1 e2 f = do
    e1' <- eval e1
    e2' <- eval e2
    case (e1',e2') of
        (PrimInt i1, PrimInt i2) -> return $ f i1 i2
        _ -> throwError $ DynamicTypeError "expected integer in expression"

-------------------------------------------------------------------------------
-- *Substitution Functions
-- $SubstitutionFunctions
--
-- Defining functions related to variable substitution.

-- |Substitutes with a given subexpression all references to a specified
--  variable in the provided expression.
subst :: Expr    -- ^ The subexpression
      -> Ident   -- ^ The identifier to replace
      -> Expr    -- ^ The expression in which to do the replacement
      -> Expr    -- ^ The resulting expression

subst v x e@(Var i)
    | i == x      = v
    | otherwise   = e

subst v x e@(Label name expr) =
    Label name $ subst v x expr

subst v x e@(Onion e1 e2) =
    Onion (subst v x e1) (subst v x e2)

subst v x e@(Func i body)
    | i == x      = e
    | otherwise   = Func i $ subst v x body

subst v x e@(Appl e1 e2) =
    Appl (subst v x e1) (subst v x e2)

subst v x e@(PrimInt i) = e

subst v x e@(PrimString s) = e

subst v x e@(PrimUnit) = e

subst v x e@(Case expr branches) =
    let expr' = subst v x expr in
    Case expr' $ map substBranch branches
    where substBranch branch@(chi,branchExpr) =
            let boundIdents =
                    case chi of
                        ChiPrim _ -> []
                        ChiLabel _ i -> [i]
                        ChiOnion i1 i2 -> [i1,i2]
                        ChiFun -> []
            in
            if elem x boundIdents then branch
                                  else (chi, subst v x branchExpr)

subst v x e@(Plus e1 e2) =
    Plus (subst v x e1) (subst v x e2)

subst v x e@(Minus e1 e2) =
    Minus (subst v x e1) (subst v x e2)

subst v x e@(Equal e1 e2) =
    Equal (subst v x e1) (subst v x e2)

