{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |A module defining a Big Bang interpreter.
-}
module Language.BigBang.Interpreter.Interpreter
( evalTop
, eval
, EvalError(..)
, EvalM
, applyBuiltins
) where

import Control.Monad.Error (Error, strMsg, throwError)
import Data.Maybe (catMaybes, maybeToList)

import Language.BigBang.Ast (Branch(..), Branches, Chi(..), Expr(..))
import Language.BigBang.Render.Display
import qualified Language.BigBang.Types.Types as T
import Language.BigBang.Types.UtilTypes
    ( Ident
    , ident
    , unIdent
    , LabelName
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
instance Display EvalError where
    makeDoc ee = case ee of
        ApplNotFunction e e' ->
            text "Attempted to apply" <+> makeDoc e <+> text "to" <+>
            makeDoc e' <+> text "but the prior is not a function"
        DynamicTypeError str ->
            -- TODO: not a string!
            text "Dynamic type error:" <+> text str
        NotClosed i ->
            text "Expression not closed for variable" <+> (text $ unIdent i)
        UnmatchedCase e brs ->
            text "Case of" <+> makeDoc e <+>
            text "cannot be matched by branches" $$
            (nest 4 $ makeDoc brs)

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
    eval $ applyBuiltins e

-- |Wraps an expression in a context where builtin names are bound
applyBuiltins :: Expr -> Expr
applyBuiltins e =
  wrapper e
  where ix = ident "x"
        iy = ident "y"
        vx = Var ix
        vy = Var iy
        builtins = [
              ("plus", Func ix $ Func iy $ Plus vx vy)
            , ("minus", Func ix $ Func iy $ Minus vx vy)
            , ("equal", Func ix $ Func iy $ Equal vx vy)
            ]
        -- Takes the builtins as defined above and returns a list of functions
        -- which use let encoding to bind the name as given above to the
        -- definition as given above.
        wrappedBuiltins :: [Expr -> Expr]
        wrappedBuiltins = map (\(x,y) z -> Appl (Func (ident x) z) y) builtins
        wrapper :: Expr -> Expr
        wrapper = foldl1 (.) wrappedBuiltins


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
        Just (Func i body) -> eval $ subst e2' i body
        _ -> throwError $ ApplNotFunction e1' e2'

eval (PrimInt i) = return $ PrimInt i

eval (PrimChar c) = return $ PrimChar c

eval PrimUnit = return $ PrimUnit

-- TODO: this is dynamic case, not static case!
eval (Case e branches) = do
    e' <- eval e
    let answers = catMaybes $ map (evalBranch e') branches
    case answers of
        [] -> throwError $ UnmatchedCase e' branches
        answer:_ -> eval answer
    where evalBranch e' (Branch mbinder chi branchExpr) =
             let mexpr = coerce chi e'
                 boundExpr = do
                   expr <- mexpr
                   return $ maybe id (\binder -> subst expr binder) mbinder $
                         branchExpr
             in
             case (chi, mexpr) of
                 -- We don't care about the matching the names because it was
                 -- ensured to be correct earlier.
                 (ChiLabel _ i, Just (Label _ lblExpr)) ->
                   fmap (subst lblExpr i) boundExpr
                 _ -> boundExpr
          coerce chi =
             case chi of
                 ChiPrim T.PrimInt -> coerceToInteger
                 ChiPrim T.PrimChar -> coerceToCharacter
                 ChiPrim T.PrimUnit -> coerceToUnit
                 ChiLabel name _ -> coerceToLabel name
                 ChiFun -> coerceToFunction
                 ChiTop -> Just

eval (Plus e1 e2) =
    evalBinop e1 e2 tryExtractInteger $ \x y -> PrimInt $ x + y

eval (Minus e1 e2) =
    evalBinop e1 e2 tryExtractInteger $ \x y -> PrimInt $ x - y

eval (Equal e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    case (e1', e2') of
        (PrimInt _, PrimInt _) -> return $ Label (labelName (if e1' == e2' then "True" else "False")) PrimUnit
        (PrimChar _, PrimChar _) -> return $ Label (labelName (if e1' == e2' then "True" else "False")) PrimUnit
        ((Func _ _), (Func _ _)) -> return $ Label (labelName (if e1' == e2' then "True" else "False")) PrimUnit
        (Label name1 expr1, Label name2 expr2) -> if name1 == name2 
                                                    then return $ Label (labelName (if expr1 == expr2 then "True" else "False")) PrimUnit 
                                                    else throwError $ DynamicTypeError "incorrect type in expression"
        (PrimUnit, PrimUnit) -> return $ Label (labelName "True") PrimUnit
        (o1@(Onion _ _), o2@(Onion _ _)) -> return $ Label (labelName (if (equalOnion s1 s2) then "True" else "False")) PrimUnit
                                            where
                                              s1 = recurseOnion o1
                                              s2 = recurseOnion o2
        ((Case _ _), _) -> error "Internal state error"
        (_, (Case _ _)) -> error "Internal state error"
        ((Appl _ _), _) -> error "Internal state error"
        (_, (Appl _ _)) -> error "Internal state error"
        ((Var _), _) -> error "Internal state error"
        (_, (Var _)) -> error "Internal state error"
        (o1@(Onion _ _), _) -> if (null $ removeType e2' s1)
                                 then return $ Label (labelName (if (equalOnion s1 [e2']) then "True" else "False")) PrimUnit
                                 else throwError $ DynamicTypeError "incorrect type in expression" 
                               where
                                   s1 = recurseOnion o1
        (_, o2@(Onion _ _)) -> if (null $ removeType e1' s2)
                                 then return $ Label (labelName (if (equalOnion [e1'] s2) then "True" else "False")) PrimUnit
                                 else throwError $ DynamicTypeError "incorrect type in expression" 
                               where
                                   s2 = recurseOnion o2
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
coerceToInteger :: Expr -> Maybe Expr
coerceToInteger e =
    coerceToType simpleIntCoerce e
    where
        simpleIntCoerce (PrimInt i) = Just $ PrimInt i
        simpleIntCoerce _ = Nothing

-- |Used to obtain a Haskell integer from an expression.  Otherwise like
--  coerceToInteger.
tryExtractInteger :: Expr -> Maybe Integer 
tryExtractInteger e =
    fmap unPrimInt $ coerceToInteger e
  where unPrimInt e' = case e' of
            PrimInt i -> i
            _ -> error "coerceToInteger returned Just non-integer"

-- |Used to obtain a character from an expression.  If necessary, this routine
--  will recurse through onions looking for a character.
coerceToCharacter :: Expr -> Maybe Expr
coerceToCharacter e =
    coerceToType simpleCharCoerce e
    where
        simpleCharCoerce (PrimChar i) = Just $ PrimChar i
        simpleCharCoerce _ = Nothing

-- |Used to obtain a unit from an expression.  If necessary, this routine
--  will recurse through onions looking for a unit.
coerceToUnit :: Expr -> Maybe Expr
coerceToUnit e =
    coerceToType simpleUnitCoerce e
    where
        simpleUnitCoerce PrimUnit = Just PrimUnit
        simpleUnitCoerce _ = Nothing

-- |Used to obtain a labeled value from an expression.  If necessary, this
-- routine will recurse through onions looking for a labeled value.
-- TODO: fix this -- it doesn't properly project ex. `A x from `A 5 & `A ()
coerceToLabel :: LabelName -> Expr -> Maybe Expr
coerceToLabel name e =
  coerceToType simpleLabelCoerce e
  where
      simpleLabelCoerce lbl@(Label name' _) = if name == name'
                                                 then Just lbl
                                                 else Nothing
      simpleLabelCoerce _ = Nothing

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
    where substBranch branch@(Branch mident chi branchExpr) =
            let boundIdents =
                    maybeToList mident ++
                    case chi of
                        ChiPrim _ -> []
                        ChiLabel _ i -> [i]
                        ChiFun -> []
                        ChiTop -> []
            in
            if elem x boundIdents
                then branch
                else Branch mident chi $ subst v x branchExpr

subst v x (Plus e1 e2) =
    Plus (subst v x e1) (subst v x e2)

subst v x (Minus e1 e2) =
    Minus (subst v x e1) (subst v x e2)

subst v x (Equal e1 e2) =
    Equal (subst v x e1) (subst v x e2)

