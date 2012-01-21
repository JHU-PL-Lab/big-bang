{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |A module defining a Big Bang interpreter.
-}
module Language.TinyBang.Interpreter.Interpreter
( evalTop
, eval
, EvalError(..)
, EvalM
, applyBuiltins
) where

import Control.Monad.Error (Error, strMsg, throwError)
import Data.Maybe (catMaybes, maybeToList)
import Data.Function (on)
import Data.List(sortBy, groupBy, intersectBy)

import Language.TinyBang.Ast
  ( Branch(..)
  , Branches
  , Chi(..)
  , Expr(..)
  , Value(..)
  , LazyOperator(..)
  , EagerOperator(..)
  , exprFromValue
  )
import Language.TinyBang.Render.Display
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.UtilTypes
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
      ApplNotFunction Value Value
    | DynamicTypeError String -- TODO: figure out what goes here
    | NotClosed Ident
    | UnmatchedCase Value Branches
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

type EvalM = Either EvalError Value
type CoerceTo a = Value -> Maybe a

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
applyBuiltins e = e
  -- wrapper e
  -- where ix = ident "x"
  --       iy = ident "y"
  --       vx = Var ix
  --       vy = Var iy
  --       builtins = [
  --             ("plus", Func ix $ Func iy $ Plus vx vy)
  --           , ("minus", Func ix $ Func iy $ Minus vx vy)
  --           , ("equal", Func ix $ Func iy $ Equal vx vy)
  --           ]
  --       -- Takes the builtins as defined above and returns a list of functions
  --       -- which use let encoding to bind the name as given above to the
  --       -- definition as given above.
  --       wrappedBuiltins :: [Expr -> Expr]
  --       wrappedBuiltins = map (\(x,y) z -> Appl (Func (ident x) z) y) builtins
  --       wrapper :: Expr -> Expr
  --       wrapper = foldl1 (.) wrappedBuiltins


-- |Evaluates a Big Bang expression.
eval :: Expr -> EvalM

eval (Var i) = throwError $ NotClosed i

eval (Label n e) = do
    e' <- eval e
    return $ VLabel n e'

eval (Onion e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    return $ VOnion e1' e2'

eval (Func i e) = return $ VFunc i e

eval (Appl e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    let e1'' = coerceToFunction e1'
    case e1'' of
        Just (VFunc i body) -> eval $ subst (exprFromValue e2') i body
        _ -> throwError $ ApplNotFunction e1' e2'

eval (PrimInt i) = return $ VPrimInt i

eval (PrimChar c) = return $ VPrimChar c

eval PrimUnit = return $ VPrimUnit

eval (Case e branches) = do
    e' <- eval e
    let answers = catMaybes $ map (evalBranch e') branches
    case answers of
        [] -> throwError $ UnmatchedCase e' branches
        answer:_ -> eval answer
    where evalBranch e' (Branch mbinder chi branchExpr) =
             let mval = coerce chi e'
                 boundExpr = do
                   val <- mval
                   return $ maybe id (\binder -> subst (exprFromValue val) binder) mbinder $
                         branchExpr
             in
             case (chi, mval) of
                 -- We don't care about the matching the names because it was
                 -- ensured to be correct earlier.
                 (ChiLabel _ i, Just (VLabel _ lblVal)) ->
                   fmap (subst (exprFromValue lblVal) i) boundExpr
                 _ -> boundExpr
          coerce chi =
             case chi of
                 ChiPrim T.PrimInt -> coerceToInteger
                 ChiPrim T.PrimChar -> coerceToCharacter
                 ChiPrim T.PrimUnit -> coerceToUnit
                 ChiLabel name _ -> coerceToLabel name
                 ChiFun -> coerceToFunction
                 ChiAny -> Just

eval (LazyOp e1 op e2) =
    evalBinop e1 e2 tryExtractInteger $
      case op of
        Plus  -> \x y -> VPrimInt $ x + y
        Minus -> \x y -> VPrimInt $ x - y

eval (EagerOp e1 op e2) = error "Eager operations are not implemented yet"

-- eval (Equal e1 e2) = do
--     e1' <- eval e1
--     e2' <- eval e2
--     case (e1', e2') of
--         (VPrimInt _, VPrimInt _) -> req e1' e2'
--         (VPrimChar _, VPrimChar _) -> req e1' e2'
--         ((VFunc _ _), (VFunc _ _)) -> req e1' e2'
--         (VLabel name1 expr1, VLabel name2 expr2) -> if name1 == name2
--                                                      then req expr1 expr2
--                                                      else throwError $ DynamicTypeError "incorrect type in expression"
--         (VPrimUnit, VPrimUnit) -> return true
-- -- FIXME: Some things that should be type errors evaluate to false.
--         (o1@(VOnion _ _), o2@(VOnion _ _)) -> oreq o1 o2
--         (o1@(VOnion _ _), _) -> ovreq o1 e2'
--         (_, o2@(VOnion _ _)) -> ovreq o2 e1'
--         _ -> throwError $ DynamicTypeError "incorrect type in expression"
--   where true  = VLabel (labelName "True" ) VPrimUnit
--         false = VLabel (labelName "False") VPrimUnit
--         eq a b = if a == b then true else false
--         req a b = return $ eq a b
--         oreq a b = return $ if onionEq a b
--                                then true
--                                else false
--         ovreq o v = return $ if onionValueEq o v
--                                 then true
--                                 else false

-- |Flattens onions to a list whose elements are guaranteed not to
--  be onions themselves and which appear in the same order as they
--  did in the original onion
flattenOnion :: Value -> [Value]
flattenOnion e =
  case e of
    VOnion e1 e2 -> flattenOnion e1 ++ flattenOnion e2
-- Removed because I think that with it present, it invokes that
-- annoying onion equality property that we can never remember is gone.
--    VLabel i e1  -> map (VLabel i) $ flattenOnion e1
    _            -> [e]

-- |Transforms a list representing a flattened onion to one containing
--  no type duplicates.
canonicalizeList :: [Value] -> [Value]
canonicalizeList xs = map last ys
  where ys = groupBy ((==) `on` intFromType) $ sortBy (compare `on` intFromType) xs

--TODO: come up with a more robust solution; perhaps a newtype?
-- |Convert type constructor to integer.
intFromType :: Value -> Int
intFromType v =
  case v of
    VLabel    _ _ -> 1
    VOnion    _ _ -> 2
    VFunc     _ _ -> 3
    VPrimInt  _   -> 4
    VPrimChar _   -> 5
    VPrimUnit     -> 6

onionEq o1 o2 = c o1 == c o2
  where c = canonicalizeList . flattenOnion

onionValueEq o v = c o == [v]
  where c = canonicalizeList . flattenOnion

{-# DEPRECATED recurseOnion, removeType, firstOfType, equalOnion "These functions are outdated" #-}
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
evalBinop :: Expr              -- ^The first argument to the binary operator.
          -> Expr              -- ^The second argument to the binary operator.
          -> CoerceTo a        -- ^A coercion function for correct arg type
          -> (a -> a -> Value) -- ^A function to evaluate coerced arguments
          -> EvalM             -- ^The results of evaluation
evalBinop e1 e2 c f = do
    e1' <- eval e1
    e2' <- eval e2
    case (c e1', c e2') of
        (Just i1, Just i2) -> return $ f i1 i2
        _ -> throwError $ DynamicTypeError "incorrect type in expression"

-- |Used to perform general coercion of values.  This function takes a direct
--  coercion function (which should be relatively trivial) and applies it
--  appropriately across onions and other such values.
coerceToType :: CoerceTo a -- ^The trivial coercion function.
             -> Value      -- ^The value to coerce.
             -> Maybe a    -- ^Just the result or Nothing on failure
coerceToType f e =
    case f e of
        Just a -> Just a
        Nothing ->
            case e of
                VOnion a b ->
                    case (coerceToType f a, coerceToType f b) of
                        (Just i, _) -> Just i
                        (Nothing, Just j) -> Just j
                        (Nothing, Nothing) -> Nothing
                _ -> Nothing

-- |Used to obtain an integer from an value.  If necessary, this routine
--  will recurse through onions looking for an integer.
coerceToInteger :: CoerceTo Value
coerceToInteger e =
    coerceToType simpleIntCoerce e
    where
        simpleIntCoerce (VPrimInt i) = Just $ VPrimInt i
        simpleIntCoerce _ = Nothing

-- |Used to obtain a Haskell integer from a value.  Otherwise like
--  coerceToInteger.
tryExtractInteger :: CoerceTo Integer
tryExtractInteger e =
    fmap unPrimInt $ coerceToInteger e
  where unPrimInt e' = case e' of
            VPrimInt i -> i
            _ -> error "coerceToInteger returned Just non-integer"

-- |Used to obtain a character from a value.  If necessary, this routine
--  will recurse through onions looking for a character.
coerceToCharacter :: CoerceTo Value
coerceToCharacter e =
    coerceToType simpleCharCoerce e
    where
        simpleCharCoerce (VPrimChar i) = Just $ VPrimChar i
        simpleCharCoerce _ = Nothing

-- |Used to obtain a unit from a value.  If necessary, this routine
--  will recurse through onions looking for a unit.
coerceToUnit :: CoerceTo Value
coerceToUnit e =
    coerceToType simpleUnitCoerce e
    where
        simpleUnitCoerce VPrimUnit = Just VPrimUnit
        simpleUnitCoerce _ = Nothing

-- |Used to obtain a labeled value from an expression.  If necessary, this
-- routine will recurse through onions looking for a labeled value.
coerceToLabel :: LabelName -> CoerceTo Value
coerceToLabel name e =
  if null coercedList
     then Nothing
     else Just coercedOnion
  where
      simpleLabelCoerce lbl@(VLabel name' _) = if name == name'
                                                  then Just lbl
                                                  else Nothing
      simpleLabelCoerce _ = Nothing
      coercedList = catMaybes $ map simpleLabelCoerce $ flattenOnion e
      coercedOnion = VLabel name $ foldr1 VOnion $ map unLabel coercedList
      unLabel (VLabel _ e') = e'
      unLabel _ = error "Found non-label in list of labels"

-- |Used to obtain a function from an expression.  If necessary, this routine
--  will recurse through onions looking for a function.
coerceToFunction :: CoerceTo Value
coerceToFunction e =
    coerceToType simpleFuncCoerce e
    where
        simpleFuncCoerce a@(VFunc _ _) = Just a
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
                        ChiAny -> []
            in
            if elem x boundIdents
                then branch
                else Branch mident chi $ subst v x branchExpr

subst v x (LazyOp e1 op e2) =
    LazyOp (subst v x e1) op (subst v x e2)

subst v x (EagerOp e1 op e2) =
    EagerOp (subst v x e1) op (subst v x e2)
