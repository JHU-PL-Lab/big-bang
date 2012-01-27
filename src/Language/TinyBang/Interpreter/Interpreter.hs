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
import Control.Arrow (second)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes, maybeToList)
import Data.Function (on)
import Data.List(foldl1', sort, sortBy, groupBy)-- intersectBy (redundant but used)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import Control.Monad.Writer (tell, listen, execWriter, Writer)

import Language.TinyBang.Ast
  ( Branch(..)
  , Branches
  , Chi(..)
  , Expr(..)
  , Value(..)
  , Assignable(..)
  , LazyOperator(..)
  , EagerOperator(..)
  , SubTerm(..)
  , CellId
  , exprFromValue
  )
import Language.TinyBang.Render.Display
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.UtilTypes
    ( Ident
    , unIdent
    , LabelName
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
        text "The value" <+> makeDoc a <+> text "can't be assigned to."

-- I'm not sure if this or the other order of the monad transformers is
-- preferable to the alternative. TODO: figure it out.
type EvalM a = StateT Map (Either EvalError) a
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
    fmap ( canonicalize . second snd) $ runStateT (eval $ applyBuiltins e) (0, IntMap.empty)

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

eval (Case e branches) = do
  v <- eval e
  let answers = catMaybes $ map (eMatch v) branches
  case answers of
    [] -> throwError $ UnmatchedCase v branches
    answer:_ -> eval answer
  where eMatch v (Branch b chi expr) =
          case (chi, v') of
            (ChiLabel _ x, Just (VLabel _ c)) -> subst (VCell c) x <$> expr'
            _ -> expr'
          where v' = eSearch v chi
                expr' =  do
                  mv <- v'
                  return $ case b of
                    Just x -> subst mv x expr
                    Nothing -> expr
        eSearch v chi =
          case (chi, v) of
            (ChiAny, _) -> Just v
            (ChiPrim T.PrimInt, VPrimInt _) -> Just v
            (ChiPrim T.PrimChar, VPrimChar _) -> Just v
            (ChiPrim T.PrimUnit, VPrimUnit) -> Just v
            (ChiLabel n _, VLabel n' _) | n == n' -> Just v
            (ChiFun, VFunc _ _) -> Just v
            (_, VOnion v1 v2) ->
              case eSearch v2 chi of
                Nothing -> eSearch v1 chi
                ret -> ret
            _ -> Nothing

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

eval (LazyOp op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (op, v1, v2) of
    (Plus,  VPrimInt x, VPrimInt y) -> return $ VPrimInt $ x + y
    (Minus, VPrimInt x, VPrimInt y) -> return $ VPrimInt $ x - y
    _ -> throwError $ DynamicTypeError "Uncaught type error in integer operation."

eval (EagerOp op e1 e2) = error "Eager operations are not implemented yet" $
  case op of
    Equal -> undefined e1
    LessEqual -> undefined e2
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

-- Still useful: commented out to silence "Defined but not used" warnings.
-- onionListLessEq _ [] _  = Just True
-- onionListLessEq _ _  [] = Nothing
-- onionListLessEq cmp (x:xs) (y:ys) =
--   case compareValues x y of
--     LT -> onionListLessEq cmp xs (y:ys)
--     EQ -> (&& cmp x y) <$> onionListLessEq cmp xs ys
--     GT -> onionListLessEq cmp (x:xs) ys

data ValueOrdinal
  = OrdLabel LabelName
  | OrdFunc
  | OrdPrimInt
  | OrdPrimChar
  | OrdPrimUnit
  deriving (Eq, Ord)

-- Still useful: commented out to silence "Defined but not used" warnings.
-- leqValues = (<=) `on` valueToOrd
compareValues :: Value -> Value -> Ordering
compareValues = compare `on` valueToOrd

eqValues :: Value -> Value -> Bool
eqValues = (==) `on` valueToOrd

valueToOrd :: Value -> ValueOrdinal
valueToOrd v =
  case v of
    VLabel n _ -> OrdLabel n
    VFunc _ _ -> OrdFunc
    VPrimInt _ -> OrdPrimInt
    VPrimChar _ -> OrdPrimChar
    VPrimUnit -> OrdPrimUnit
    _ -> error "This value should not be inside an onion"

-- Still useful: commented out to silence "Defined but not used" warnings.
-- onionEq :: Value -> Value -> Bool
-- onionEq o1 o2 = c o1 == c o2
--   where c = canonicalizeList . flattenOnion

-- onionValueEq :: Value -> Value -> Bool
-- onionValueEq o v = c o == [v]
--   where c = canonicalizeList . flattenOnion

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
  | i == x    = Assign (AValue v) (subst v x e1) (subst v x e2)
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
  (valueRemap v, imapRemap imap)
  where -- FIXME: Make more efficient later if neccessary
        gatherCells :: Value -> Writer [CellId] ()
        gatherCells v' = do
          case v' of
            VLabel _ c -> tell [c]
            VOnion v1 v2 -> gatherCells v1 >> gatherCells v2
            _ -> return ()
        followCellRefs :: Value -> Writer [CellId] ()
        followCellRefs v' = do
          cs <- snd <$> listen (gatherCells v')
          mapM_ followCellRefs $ map (imap !) cs
        remap :: IntMap CellId
        remap = IntMap.fromList $ (\x -> zip (map fst x) [0 :: Int ..]) $
                  IntMap.toList $ IntMap.fromListWith (flip const) $
                  zip (execWriter $ followCellRefs v) [0 :: Int ..]
        valueRemap v' =
          case v' of
            VLabel n c -> VLabel n $ remap ! c
            VOnion v1 v2 -> VOnion (valueRemap v1) (valueRemap v2)
            _ -> v'
        imapRemap imap' =
          IntMap.fromList $ map pairRemap $
            filter ((`IntMap.member` remap) . fst) $ IntMap.assocs imap'
        pairRemap (cell, contents) =
          (remap ! cell, valueRemap contents)
