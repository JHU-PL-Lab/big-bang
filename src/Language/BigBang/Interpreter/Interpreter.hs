{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |A module defining a Big Bang interpreter.
-}
module Language.BigBang.Interpreter.Interpreter
( evalTop
, eval
, EvalError(..)
, EvalM
) where

import Control.Monad.Error (Error, strMsg, throwError)
import Data.List (foldl')
import Data.Maybe (catMaybes)

import qualified Data.Set as Set

import Language.BigBang.Ast (Branches, Chi(..), Expr(..))
import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Types.UtilTypes
    ( Ident
    , ident
--    , unIdent
--    , LabelName
    , labelName
--    , unLabelName
    )

-- TODO: remove
-- import Debug.Trace

-- |An error type for evaluation failures.
data EvalError =
      ApplNotFunction Expr Expr
    | DynamicTypeError String -- TODO: figure out what goes here
    | NotClosed Ident
    | UnmatchedCase Expr Branches
    deriving (Eq, Show)
instance Error EvalError where
    strMsg = error

type EvalM = Either EvalError Expr

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
    let e1'' = coerceToFunction e1'
    case e1'' of
        Just (Func ident body) -> eval $ subst e2' ident body
        _ -> throwError $ ApplNotFunction e1' e2'

eval (PrimInt i) = return $ PrimInt i

eval (PrimChar c) = return $ PrimChar c

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
                  (ChiPrim T.PrimChar, PrimChar _) -> Just branchExpr
                  (ChiPrim T.PrimUnit, PrimUnit) -> Just branchExpr
                  (ChiLabel name ident, Label name' lblExpr) ->
                      if name == name'
                        then Just $ subst lblExpr ident branchExpr
                        else Nothing
                  (ChiFun, Func _ _) -> Just branchExpr
                  (ChiTop, _) -> Just branchExpr
                  _ -> Nothing

eval (Plus e1 e2) =
    evalBinop e1 e2 coerceToInteger $ \x y -> PrimInt $ x + y

eval (Minus e1 e2) =
    evalBinop e1 e2 coerceToInteger $ \x y -> PrimInt $ x - y

eval (Equal e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    let
        returnVal = return $ Label (labelName (if e1' == e2' then "True" else "False")) PrimUnit
        errorMsg = error "Internal state error"
        in case (e1', e2') of
            (PrimInt _, PrimInt _) -> returnVal
            (PrimChar _, PrimChar _) -> returnVal
            ((Func _ _), (Func _ _)) -> returnVal

            (Label name1 expr1, Label name2 expr2) -> 
                if name1 == name2 
                  then return $ Label (labelName (if expr1 == expr2 then "True" else "False")) PrimUnit 
                  else throwError $ DynamicTypeError "incorrect type in expression"

            (PrimUnit, PrimUnit) -> return $ Label (labelName "True") PrimUnit

            (o1@(Onion _ _), o2@(Onion _ _)) -> return $ Label (labelName (if (equalOnion s1 s2) then "True" else "False")) PrimUnit
                                                  where
                                                      s1 = recurseOnion o1
                                                      s2 = recurseOnion o2

            ((Case _ _), _) -> errorMsg
            (_, (Case _ _)) -> errorMsg
            ((Appl _ _), _) -> errorMsg
            (_, (Appl _ _)) -> errorMsg
            ((Var _), _) -> errorMsg
            (_, (Var _)) -> errorMsg
            _ -> throwError $ DynamicTypeError "incorrect type in expression" 

recurseOnion :: Expr -> [Expr]
recurseOnion (Onion e1 e2) = recurseOnion e1 ++ recurseOnion e2
recurseOnion x = [x]

removeType :: Expr -> [Expr] -> [Expr]
removeType _ [] = []
removeType e@(Var _) ((Var _):xs) = removeType e xs
removeType e@(Label _ _) ((Label _ _):xs) = removeType e xs
removeType e@(Func _ _) ((Func _ _):xs) = removeType e xs
removeType e@(Appl _ _) ((Appl _ _):xs) = removeType e xs
removeType e@(PrimInt _) ((PrimInt _):xs) = removeType e xs
removeType e@(PrimChar _) ((PrimChar _):xs) = removeType e xs
removeType e@PrimUnit (PrimUnit:xs) = removeType e xs
removeType e@(Case _ _) ((Case _ _):xs) = removeType e xs
removeType _ ((Onion _ _):_) = error "Internal state error"
removeType e (x:xs) = x : removeType e xs 

firstOfType :: Expr -> [Expr] -> Maybe Expr
firstOfType _ [] = Nothing
firstOfType (Var _) (x@(Var _):_) = Just x
firstOfType (Label _ _) (x@(Label _ _):_) = Just x
firstOfType (Func _ _) (x@(Func _ _):_) = Just x
firstOfType (Appl _ _) (x@(Appl _ _):_) = Just x
firstOfType (PrimInt _) (x@(PrimInt _):_) = Just x
firstOfType (PrimChar _) (x@(PrimChar _):_) = Just x
firstOfType PrimUnit (x@PrimUnit:_) = Just x
firstOfType (Case _ _) (x@(Case _ _):_) = Just x
firstOfType _ ((Onion _ _):_) = error "Internal state error"
firstOfType e (_:xs) = firstOfType e xs

equalOnion :: [Expr] -> [Expr] -> Bool
equalOnion [] [] = True
equalOnion [] _ = False
equalOnion _ [] = False
equalOnion (x:xs) ys = let ys' = removeType x ys
                           xs' = removeType x xs
                           v = firstOfType x ys
                       in case v of
                         Nothing -> False
                         Just y -> ((x == y) && equalOnion xs' ys')

-- |Evaluates a binary expression.
evalBinop :: Expr -- ^The first argument to the binary operator.
          -> Expr -- ^The second argument to the binary operator.
          -> (Expr -> Maybe a) -- ^A coercion function for correct arg type
          -> (a -> a -> Expr) -- ^A function to evaluate coerced arguments
          -> EvalM -- ^The results of evaluation
evalBinop e1 e2 c f = do
    e1' <- eval e1
    e2' <- eval e2
    case (c e1', c e2') of
        (Just i1, Just i2) -> return $ f i1 i2
        _ -> throwError $ DynamicTypeError "incorrect type in expression"

-- TODO: all of this coercion logic is dynamic; problem?

-- |Used to perform general coercion of values.  This function takes a direct
--  coercion function (which should be relatively trivial) and applies it
--  appropriately across onions and other such values.
coerceToType :: (Expr -> Maybe a) -- ^The trivial coercion function.
             -> Expr              -- ^The expression to coerce.
             -> Maybe a           -- ^Just the result or Nothing on failure
coerceToType f e =
    case f e of
        Just a -> Just a
        Nothing ->
            case e of
                Onion a b ->
                    case (coerceToType f a, coerceToType f b) of
                        (Just i, _) -> Just i
                        (Nothing, Just j) -> Just j
                        (Nothing, Nothing) -> Nothing
                _ -> Nothing

-- |Used to obtain an integer from an expression.  If necessary, this routine
--  will recurse through onions looking for an integer.
coerceToInteger :: Expr -> Maybe Integer
coerceToInteger e =
    coerceToType simpleIntCoerce e
    where
        simpleIntCoerce (PrimInt i) = Just i
        simpleIntCoerce _ = Nothing

-- |Used to obtain a function from an expression.  If necessary, this routine
--  will recurse through onions looking for a function.
coerceToFunction :: Expr -> Maybe Expr
coerceToFunction e =
    coerceToType simpleFuncCoerce e
    where
        simpleFuncCoerce a@(Func _ _) = Just a
        simpleFuncCoerce _ = Nothing

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

subst v x (Label name expr) =
    Label name $ subst v x expr

subst v x (Onion e1 e2) =
    Onion (subst v x e1) (subst v x e2)

subst v x e@(Func i body)
    | i == x      = e
    | otherwise   = Func i $ subst v x body

subst v x (Appl e1 e2) =
    Appl (subst v x e1) (subst v x e2)

subst _ _ e@(PrimInt _) = e

subst _ _ e@(PrimChar _) = e

subst _ _ e@(PrimUnit) = e

subst v x (Case expr branches) =
    let expr' = subst v x expr in
    Case expr' $ map substBranch branches
    where substBranch branch@(chi,branchExpr) =
            let boundIdents =
                    case chi of
                        ChiPrim _ -> []
                        ChiLabel _ i -> [i]
                        ChiFun -> []
                        ChiTop -> []
            in
            if elem x boundIdents then branch
                                  else (chi, subst v x branchExpr)

subst v x (Plus e1 e2) =
    Plus (subst v x e1) (subst v x e2)

subst v x (Minus e1 e2) =
    Minus (subst v x e1) (subst v x e2)

subst v x (Equal e1 e2) =
    Equal (subst v x e1) (subst v x e2)

