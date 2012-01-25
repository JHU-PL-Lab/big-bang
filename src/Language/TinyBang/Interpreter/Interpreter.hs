{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

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
import Control.Monad.State (StateT, runStateT, get, put, gets, modify)
import Control.Arrow (first, second)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes, maybeToList)
import Data.Function (on)
import Data.List(foldl1', sort, sortBy, groupBy)-- intersectBy (redundant but used)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import Data.Maybe (listToMaybe)
import Data.Maybe.Utils (justIf)

import Language.TinyBang.Ast
  ( Branch(..)
  , Branches
  , Chi(..)
  , Expr(..)
  , Value(..)
  , Assignable(..)
  , LazyOperator(..)
  , EagerOperator(..)
  , Sigma(..)
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
    | IllegalComparison EagerOperator Value Value
    | IllegalAssignment Assignable
    deriving (Eq, Show)
instance Error EvalError where
  strMsg = error
instance Display EvalError where
  makeDoc ee =
    case ee of
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
      IllegalComparison op v1 v2 ->
        text "The comparison" <+> makeDoc op <+> makeDoc v1 <+> makeDoc v2 <+>
        text "cannot be is not well typed."
      IllegalAssignment a ->
        makeDoc a <+> text "Can't be assigned to."

-- I'm not sure if this or the other order of the monad transformers is
-- preferable to the alternative. TODO: figure it out.
type EvalM a = StateT Map (Either EvalError) a
type CoerceTo a = Value -> Maybe a
type Cell = Int
type NextCell = Int
type Map = (NextCell, IntMap Value)
type Result = (Value, IntMap Value)

newCell :: Value -> EvalM Cell
newCell v = do
  (i, m) <- get
  put (i + 1, IntMap.insert i v m)
  return i

readCell :: Cell -> EvalM Value
readCell i = gets snd >>= return . (! i)

writeCell :: Cell -> Value -> EvalM ()
writeCell i v = modify (second $ IntMap.adjust (const v) i)

------------------------------------------------------------------------------
-- *Evaluation Functions
-- $EvaluationFunctions
--
-- Definitions for functions related to expression evaluation.

-- |Performs top-level evaluation of a Big Bang expression.  This evaluation
--  routine binds built-in functions (like "plus") to the appropriate
--  expressions.
evalTop :: Expr -> Either EvalError Result
evalTop e =
    fmap ( {-canonicalize . -} second snd) $ runStateT (eval $ applyBuiltins e) (0, IntMap.empty)

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

onion :: Value -> Value -> Value
onion VEmptyOnion v = v
onion v VEmptyOnion = v
onion v1 v2 = VOnion v1 v2

-- |Evaluates a Big Bang expression.
eval :: Expr -> EvalM Value

-- The next four cases are covered by the value rule
eval (Func i e) = return $ VFunc i e
eval (PrimInt i) = return $ VPrimInt i
eval (PrimChar c) = return $ VPrimChar c
eval PrimUnit = return $ VPrimUnit

eval (Var i) = throwError $ NotClosed i

eval (ExprCell c) = readCell c

eval (Label n e) = do
  e' <- eval e
  c <- newCell e'
  return $ VLabel n c

eval (Onion e1 e2) = do
  e1' <- eval e1
  e2' <- eval e2
  return $ onion e1' e2'

eval (OnionSub e s) = do
  v <- eval e
  return $ onionSub v
  where onionSub v =
          case (v, s) of
            (VOnion v1 v2, _) -> onion (onionSub v1) (onionSub v2)
            (VPrimInt _, SubPrim T.PrimInt) -> VEmptyOnion
            (VPrimChar _, SubPrim T.PrimChar) -> VEmptyOnion
            (VPrimUnit, SubPrim T.PrimUnit) -> VEmptyOnion
            (VFunc _ _, SubFunc) -> VEmptyOnion
            (VLabel n _, SubLabel n') | n == n' -> VEmptyOnion
            _ -> v

eval (Appl e1 e2) = do
  e1' <- eval e1
  e2' <- eval e2
  case e1' of
    VFunc i body -> eval $ subst e2' i body
    _ -> throwError $ ApplNotFunction e1' e2'

--TODO: define an duse eMatch
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
                return $ maybe id (\b -> subst val b) mbinder $ branchExpr
          in
          case (chi, mval) of
            -- We don't care about the matching the names because it was
            -- ensured to be correct earlier.
            (ChiLabel _ i, Just (VLabel _ lblVal)) ->
              subst (VCell lblVal) i <$> boundExpr
            _ -> boundExpr
        coerce chi =
          case chi of
            ChiPrim T.PrimInt -> coerceToInteger
            ChiPrim T.PrimChar -> coerceToCharacter
            ChiPrim T.PrimUnit -> coerceToUnit
            ChiLabel name _ -> coerceToLabel name
            ChiFun -> coerceToFunction
            ChiAny -> Just

eval (Def i e1 e2) = do
  e1' <- eval e1
  cellId <- newCell e1'
  eval $ subst (VCell cellId) i e2

eval (Assign a e1 e2) =
  case a of
    AIdent i -> throwError $ NotClosed i
    AValue (VCell c) -> do
      e1' <- eval e1
      writeCell c e1'
      eval e2
    _ -> throwError $ IllegalAssignment a

eval (LazyOp op e1 e2) =
  evalBinop e1 e2 tryExtractInteger $
    case op of
      Plus  -> \x y -> VPrimInt $ x + y
      Minus -> \x y -> VPrimInt $ x - y

eval (EagerOp op e1 e2) = error "Eager operations are not implemented yet" $
  case op of
    Equal -> undefined
    LessEqual -> undefined
    GreaterEqual -> undefined

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
    VEmptyOnion -> []
    VOnion e1 e2 -> flattenOnion e1 ++ flattenOnion e2
-- Removed because I think that with it present, it invokes that
-- annoying onion equality property that we can never remember is gone.
--    VLabel i e1  -> map (VLabel i) $ flattenOnion e1
    _            -> [e]

-- |Transforms a list representing a flattened onion to one containing
--  no type duplicates.
canonicalizeList :: [Value] -> [Value]
canonicalizeList xs = map last ys
  where ys = groupBy eqValues $ sortBy compareValues xs

-- |Transforms an onion into canonical form.  Canonical form requires
--  that there be no duplicate labels, that the onion be left leaning,
--  and that the onion entries be sorted in accordance with the ordering
--  defined over Values
canonicalizeOnion :: Value -> Value
canonicalizeOnion = foldl1' VOnion . sort . canonicalizeList . flattenOnion

onionListLessEq _ [] _  = Just True
onionListLessEq _ _  [] = Nothing
onionListLessEq cmp (x:xs) (y:ys) =
  case compareValues x y of
    LT -> onionListLessEq cmp xs (y:ys)
    EQ -> (&& cmp x y) <$> onionListLessEq cmp xs ys
    GT -> onionListLessEq cmp (x:xs) ys

data ValueOrdinal
  = OrdLabel LabelName
  | OrdFunc
  | OrdPrimInt
  | OrdPrimChar
  | OrdPrimUnit
  deriving (Eq, Ord)

leqValues = (<=) `on` valueToOrd
compareValues = compare `on` valueToOrd
eqValues = (==) `on` valueToOrd

valueToOrd v =
  case v of
    VLabel n _ -> OrdLabel n
    VFunc _ _ -> OrdFunc
    VPrimInt _ -> OrdPrimInt
    VPrimChar _ -> OrdPrimChar
    VPrimUnit -> OrdPrimUnit
    _ -> error "This value should not be inside an onion"

onionEq :: Value -> Value -> Bool
onionEq o1 o2 = c o1 == c o2
  where c = canonicalizeList . flattenOnion

onionValueEq :: Value -> Value -> Bool
onionValueEq o v = c o == [v]
  where c = canonicalizeList . flattenOnion

-- |Evaluates a binary expression.
evalBinop :: Expr              -- ^The first argument to the binary operator.
          -> Expr              -- ^The second argument to the binary operator.
          -> CoerceTo a        -- ^A coercion function for correct arg type
          -> (a -> a -> Value) -- ^A function to evaluate coerced arguments
          -> EvalM Value       -- ^The results of evaluation
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
    onionCoerce
  where
      labelMatch (VLabel name' _) = name == name'
      labelMatch _ = False
      simpleLabelCoerce lbl = lbl `justIf` (labelMatch lbl)
      onionCoerce = listToMaybe $ reverse $ filter labelMatch coercedList
      coercedList = catMaybes $ map simpleLabelCoerce $ flattenOnion e

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



-- |Substitutes with a given cell all references to a specified
--  variable in the provided expression.
subst :: Value   -- ^ The value
      -> Ident   -- ^ The identifier to replace
      -> Expr    -- ^ The expression in which to do the replacement
      -> Expr    -- ^ The resulting expression

subst v x e@(Var i)
  | i == x      = exprFromValue v
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

subst v x (OnionSub e s) =
  OnionSub (subst v x e) s

subst v x (LazyOp op e1 e2) =
  LazyOp op (subst v x e1) (subst v x e2)

subst v x (EagerOp op e1 e2) =
  EagerOp op (subst v x e1) (subst v x e2)

subst v x (Def i e1 e2)
  | i == x    = Def i (subst v x e1) e2
  | otherwise = Def i (subst v x e1) (subst v x e2)

subst v x (Assign c@(AValue _) e1 e2) =
  Assign c (subst v x e1) (subst v x e2)

subst v x (Assign ai@(AIdent i) e1 e2)
  | i == x    = Assign (AValue v) (subst v x e1) e2
  | otherwise = Assign ai (subst v x e1) (subst v x e2)

subst _ _ e@(ExprCell _) = e

-- |This function takes a value-mapping pair and returns a new one in
--  canonical form.  Canonical form requires that there be no repeated
--  labels, no unreachable cells, and that the cells' names are
--  determined by position in the value.
canonicalize :: Result -> Result
canonicalize (v, imap) = canonicalize' (v', imap)
  where v' = case v of
               VOnion _ _ -> canonicalizeOnion v
               _ -> v

-- |Helper function for 'canonicalize', which assumes that its input has
--  been deduped if it's an onion.
canonicalize' :: Result -> Result
canonicalize' (v, imap) =
  case v of
    VLabel lbl cell -> (VLabel lbl 0, IntMap.singleton 0 (imap ! cell))
    VOnion v1 v2 ->
      let (c1, m1) = canonicalize' (v1, imap)
          (c2, m2) = canonicalize' (v2, imap)
          o = VOnion c1 c2
          m1list = IntMap.toList m1
          m2list = IntMap.toList m2
          len = length m1list
      in case (IntMap.null m1, IntMap.null m2) of
           (True , True ) -> (o, IntMap.empty)
           (False, True ) -> (o, m1)
           (True , False) -> (o, m2)
           (False, False) ->
             -- Ensure that keys don't collide by adding an offset equal
             -- to the size of the first list to each key
             (VOnion c1 $ incCells len c2, IntMap.fromList $
                m1list ++ (map (first (+ len)) m2list))
    _ -> (v, IntMap.empty)
  where incCells i v' = case v' of
            VLabel lbl c -> VLabel lbl $ c + i
            VOnion e1 e2 -> VOnion (incCells i e1) (incCells i e2)
            _ -> v
