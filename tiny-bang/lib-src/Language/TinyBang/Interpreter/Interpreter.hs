{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             TupleSections,
             ImplicitParams,
             GADTs
             #-}

{- |A module defining a Big Bang interpreter.
-}
module Language.TinyBang.Interpreter.Interpreter
( evalTop
, eval
, EvalError(..)
, EvalM
, applyBuiltins
, canonicalize
, onion
) where

import Control.Monad.Error (Error, strMsg, throwError)
import Control.Monad.State (StateT, runStateT, get, put, gets, modify)
import Control.Monad.Reader (ReaderT, Reader, asks, ask, runReader)
import Control.Monad.Identity (Identity)
import Control.Arrow (second)
import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List(foldl1', sort, sortBy, groupBy, nubBy)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import Control.Monad.Writer (tell, listen, execWriter, Writer)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
  ( Branch(..)
  , Branches
  , Chi(..)
  , ChiPrimary
  , ChiStruct
  , Expr(..)
  , Value(..)
  , Assignable(..)
  , LazyOperator(..)
  , EagerOperator(..)
  , SubTerm(..)
  , CellId
  , ePatVars
  )
import qualified Language.TinyBang.Types.Types as T
import Language.TinyBang.Types.UtilTypes
    ( Ident
    , unIdent
    , LabelName
    , labelName
    )
import Utils.Render.Display

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
type EvalM a = StateT CellMap (Either EvalError) a
type EnvReader a = Reader CellMap a
type Cell = Int
type NextCell = Int
type CellMap = (NextCell, IntMap Value)
type Result = (Value, IntMap Value)

class CellReadable m where
  readCell :: Cell -> m Value

instance CellReadable (StateT CellMap (Either EvalError)) where
  readCell i = gets snd >>= return . (! i)

instance CellReadable (ReaderT CellMap Identity) where
  readCell i = asks snd >>= return . (! i)

runEvalMReader :: EnvReader a -> EvalM a
runEvalMReader envReader = get >>= return . runReader envReader

newCell :: Value -> EvalM Cell
newCell v = do
  (i, m) <- get
  put (i + 1, IntMap.insert i v m)
  return i

--readCell :: Cell -> EvalM Value
--readCell i = gets snd >>= return . (! i)

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
evalTop :: (?debug :: Bool) => Expr -> Either EvalError Result
evalTop e =
    fmap (second snd) $ runStateT (eval $ applyBuiltins e) (0, IntMap.empty)

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

type IdMap = Map Ident CellId

-- |Evaluates a Big Bang expression.
eval :: (?debug :: Bool) => Expr -> EvalM Value

-- The next four cases are covered by the value rule
eval e = do
  case e of
      Func i e' -> return $ VFunc i e'
      PrimInt i -> return $ VPrimInt i
      PrimChar c -> return $ VPrimChar c
      PrimUnit -> return $ VPrimUnit
      Var i -> throwError $ NotClosed i
      ExprCell c -> readCell c
      Label n e' -> do
        v <- eval e'
        c <- newCell v
        return $ VLabel n c
      Onion e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        return $ onion v1 v2
      OnionSub e' s -> do
        v <- eval e'
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
      EmptyOnion -> return $ VEmptyOnion
      Appl e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        cellId <- newCell v2
        case v1 of
          VFunc i body -> eval $ subst cellId i body
          _ -> throwError $ ApplNotFunction v1 v2
      Case e' branches -> do
        v <- eval e'
--        let answers = mapMaybe (eMatch v) branches
        let findAnswer bs =
              case bs of
                [] -> throwError $ UnmatchedCase v branches
                b:bs' -> do
                  mExpr <- eMatch v b
                  case mExpr of
                    Nothing -> findAnswer bs'
                    Just expr -> return expr
        eval =<< findAnswer branches
        where eMatch :: Value -> Branch -> EvalM (Maybe Expr)
              eMatch v (Branch chi e1) = do
                m <- eSearch v chi
                return $ do
                  (_, b) <- m
                  return $ eSubstAll e1 b
              eSubstAll :: Expr -> IdMap -> Expr
              eSubstAll expr b = Map.foldrWithKey (flip subst) expr b
              eSearch :: Value
                      -> Chi a
                      -> EvalM (Maybe (Value, IdMap))
              eSearch v chi =
                case chi of
                  ChiSimple mx -> Just . (v,) <$> mapMaybeVar mx
                  ChiComplex chiBind -> eSearch v chiBind
                  chiStruct@(ChiOnionOne {}) -> searchAll chiStruct
                  chiStruct@(ChiOnionMany {}) -> searchAll chiStruct
                  ChiParen mx chiBound -> do
                    mPair <- eSearch v chiBound
                    idMap <- mapMaybeVar mx
                    return $ addBinding idMap <$> mPair
                  ChiPrimary mx chiBound -> do
                    mPair <- eSearch v chiBound
                    idMap <- mapMaybeVar mx
                    return $ addBinding idMap <$> mPair
                  ChiPrim prim -> recurseSearch v chi $ return . (matchPrim prim)
                  ChiLabelSimple lbl mx ->
                    recurseSearch v chi $ matchLabelSimple lbl mx
                  ChiLabelComplex lbl chiBind ->
                    recurseSearch v chi $ matchLabelComplex lbl chiBind
                  ChiFun -> recurseSearch v chi $ return . Just . (,Map.empty)
                where mapMaybeVar :: Maybe Ident -> EvalM IdMap
                      mapMaybeVar mx =
                        case mx of
                          Nothing -> return Map.empty
                          Just x -> Map.singleton x <$> newCell v
                      addBinding :: IdMap -> (Value, IdMap) -> (Value, IdMap)
                      addBinding b (v', bs) =
                        (v', bs `Map.union` b)
                      -- | Takes a (possibly onion) value, a primary pattern,
                      --   and a function to evaluate non-onion values to a
                      --   result.  Handles breaking onion values apart to find
                      --   the first matching result.
                      recurseSearch :: Value -> ChiPrimary
                                    -> (Value -> EvalM (Maybe (Value, IdMap)))
                                    -> EvalM (Maybe (Value, IdMap))
                      recurseSearch v' chiBound f =
                        case v' of
                          VOnion vLeft vRight -> do
                            -- Try to match the right side
                            mResult <- eSearch vRight chiBound
                            case mResult of
                              -- If it works, we're done
                              Just _ -> return mResult
                              -- Otherwise, try the left side
                              Nothing -> eSearch vLeft chiBound
                          -- If the value is not an onion, it's a singleton.
                          _ -> f v'
                      matchPrim :: T.PrimitiveType -> Value -> Maybe (Value, IdMap)
                      matchPrim prim v' =
                        case (prim, v') of
                          (T.PrimInt, VPrimInt _) -> yes
                          (T.PrimChar, VPrimChar _) -> yes
                          (T.PrimUnit, VPrimUnit) -> yes
                          _ -> no
                          where yes = Just (v', Map.empty)
                                no = Nothing
                      matchLabelSimple :: LabelName -> Maybe Ident -> Value
                                       -> EvalM (Maybe (Value, IdMap))
                      matchLabelSimple lbl mx v' =
                        return $ case v' of
                          VLabel lbl' c | lbl == lbl' ->
                            Just (v', maybe
                                       Map.empty
                                       (\x -> Map.singleton x c)
                                       mx)
                          _ -> Nothing
                      matchLabelComplex :: LabelName -> Chi a -> Value
                                        -> EvalM (Maybe (Value, IdMap))
                      matchLabelComplex lbl chiBind v' =
                        do -- EvalM monad
                         mPair <- (
                            case v' of
                              VLabel lbl' c0 | lbl == lbl' ->
                                flip eSearch chiBind =<< readCell c0
                              _ -> return Nothing
                            )
                         case mPair of
                           Just pair -> Just <$> newLabel pair
                           Nothing -> return Nothing
                         where newLabel (v'',m) = do
                                 c1 <- newCell v''
                                 return (VLabel lbl c1, m)
                      searchAll :: ChiStruct -> EvalM (Maybe (Value, IdMap))
                      searchAll chiStruct =
                        case chiStruct of
                          ChiOnionOne chiBind -> eSearch v chiBind
-- TODO: Possibly do something with fold rather than explicit recursion
                          ChiOnionMany chiBind chiStruct' -> do -- EvalM
                            mLeft <- eSearch v chiBind
                            mRest <- searchAll chiStruct'
                            return $ do -- Maybe
                              (vLeft, idMapLeft) <- mLeft
                              (vRest, idMapRest) <- mRest
                              return $ ( onion vLeft vRest
                                       , Map.union idMapRest idMapLeft
                                       )
      Def i e1 e2 -> do
        v1 <- eval e1
        cellId <- newCell v1
        eval $ subst cellId i e2
      Assign a e1 e2 -> do
        case a of
          AIdent i -> throwError $ NotClosed i
          ACell c -> do
            v1 <- eval e1
            writeCell c v1
            eval e2
      LazyOp op e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        case (op, v1, v2) of
          (Plus,  VPrimInt x, VPrimInt y) -> return $ VPrimInt $ x + y
          (Minus, VPrimInt x, VPrimInt y) -> return $ VPrimInt $ x - y
          _ -> throwError $ DynamicTypeError "Uncaught type error in integer operation."
      EagerOp op e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        case op of
          Equal -> eEqual v1 v2
          LessEqual -> eLessEq v1 v2
          GreaterEqual -> eGreaterEq v1 v2
        where eEqual :: (?debug :: Bool)
                     => Value -> Value -> EvalM Value
              eEqual v1 v2 = do
                c <- newCell VPrimUnit
                b1 <- runEvalMReader $ eCompare v1 v2
                b2 <- runEvalMReader $ eCompare v2 v1
                let n = if b1 && b2 then "True" else "False"
                return $ VLabel (labelName n) c
              eLessEq :: Value -> Value -> EvalM Value
              eLessEq v1 v2 = do
                c <- newCell VPrimUnit
                b <- runEvalMReader $ eCompare v1 v2
                let n = if b then "True" else "False"
                return $ VLabel (labelName n) c
              eGreaterEq :: Value -> Value -> EvalM Value
              eGreaterEq v1 v2 = eLessEq v2 v1
              eCompare :: (?debug :: Bool)
                       => Value -> Value -> EnvReader Bool
              eCompare v1 v2 = do
                env <- ask
                let cmp x y = runReader (eAtomOrder x y) env
                eListLessEq
                  (reverse $ sortBy cmp $ eFilter $ eFlatten v1)
                  (reverse $ sortBy cmp $ eFilter $ eFlatten v2)
              eAtomOrder :: (?debug :: Bool)
                         => Value -> Value -> EnvReader Ordering
              eAtomOrder v1 v2 = do
                b1 <- eAtomCompare v1 v2
                b2 <- eAtomCompare v2 v1
                case (b1,b2) of
                  (True,True) -> return EQ
                  (True,False) -> return LT
                  (False,True) -> return GT
                  (False,False) ->
                    error $ "eAtomCompare returned false for <= and >= "++
                            "of arguments (" ++ display v1 ++ "), " ++
                            "(" ++ display v2 ++ ")"
              eFilter :: [Value] -> [Value]
              eFilter vs = reverse $ nubBy eTestMatch (reverse vs)
              eTestMatch :: Value -> Value -> Bool
              eTestMatch v1 v2 =
                case (v1,v2) of
                  (VPrimUnit,VPrimUnit) -> True
                  (VPrimInt _,VPrimInt _) -> True
                  (VPrimChar _,VPrimChar _) -> True
                  (VLabel n _, VLabel n' _) -> n == n'
                  (VFunc _ _, VFunc _ _) -> True
                  _ -> False
              eListLessEq :: [Value] -> [Value] -> EnvReader Bool
              eListLessEq vs1 vs2 = do
                case (vs1,vs2) of
                  ([],_) -> return True
                  (_,[]) -> return False
                  (v1:r1,v2:r2) -> do
                    ord <- eAtomOrder v1 v2
                    case ord of
                      LT -> return True
                      GT -> return False
                      EQ -> eListLessEq r1 r2
              eAtomCompare :: Value -> Value -> EnvReader Bool
              eAtomCompare v1 v2 =
                case (v1,v2) of
                  (VPrimUnit, VPrimUnit) -> return True
                  (VPrimInt p1, VPrimInt p2) -> return $ p1 <= p2
                  (VPrimChar p1, VPrimChar p2) -> return $ p1 <= p2
                  (VLabel n1 c1, VLabel n2 c2) | n1 == n2 -> do
                    v1' <- readCell c1
                    v2' <- readCell c2
                    eCompare v1' v2'
                  (VFunc _ _, VFunc _ _) -> return $ v1 == v2
                  _ | (valueToOrd v1) < (valueToOrd v2) -> return True
                  _ -> return False

-- |Flattens onions to a list whose elements are guaranteed not to
--  be onions themselves and which appear in the same order as they
--  did in the original onion
eFlatten :: Value -> [Value]
eFlatten e =
  case e of
    VEmptyOnion -> []
    VOnion e1 e2 -> eFlatten e1 ++ eFlatten e2
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
canonicalizeOnion = foldl1' onion . sort . canonicalizeList . eFlatten

-- Still useful: commented out to silence "Defined but not used" warnings.
-- onionListLessEq _ [] _  = Just True
-- onionListLessEq _ _  [] = Nothing
-- onionListLessEq cmp (x:xs) (y:ys) =
--   case compareValues x y of
--     LT -> onionListLessEq cmp xs (y:ys)
--     EQ -> (&& cmp x y) <$> onionListLessEq cmp xs ys
--     GT -> onionListLessEq cmp (x:xs) ys

data ValueOrdinal
  = OrdPrimUnit
  | OrdPrimInt
  | OrdPrimChar
  | OrdLabel LabelName
  | OrdFunc
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
    VPrimUnit -> OrdPrimUnit
    VPrimInt _ -> OrdPrimInt
    VPrimChar _ -> OrdPrimChar
    VLabel n _ -> OrdLabel n
    VFunc _ _ -> OrdFunc
    _ -> error "This value should not be inside an onion"

-- Still useful: commented out to silence "Defined but not used" warnings.
-- onionEq :: Value -> Value -> Bool
-- onionEq o1 o2 = c o1 == c o2
--   where c = canonicalizeList . eFlatten

-- onionValueEq :: Value -> Value -> Bool
-- onionValueEq o v = c o == [v]
--   where c = canonicalizeList . eFlatten

-------------------------------------------------------------------------------
-- *Substitution Functions
-- $SubstitutionFunctions
--
-- Defining functions related to variable substitution.



-- |Substitutes with a given cell all references to a specified
--  variable in the provided expression.
subst :: CellId  -- ^ The cell
      -> Ident   -- ^ The identifier to replace
      -> Expr    -- ^ The expression in which to do the replacement
      -> Expr    -- ^ The resulting expression

subst c x e =
  case e of
    Var i ->
      if i == x then ExprCell c
                else e
    Label n e' -> Label n $ subst c x e'
    Onion e1 e2 -> Onion (subst c x e1) (subst c x e2)
    Func i e' ->
      if i == x then e
                else Func i $ subst c x e'
    Appl e1 e2 -> Appl (subst c x e1) (subst c x e2)
    PrimInt _ -> e
    PrimChar _ -> e
    PrimUnit -> e
    Case e' branches ->
      let e'' = subst c x e' in
      Case e'' $ map substBranch branches
      where substBranch branch@(Branch chi branchExpr) =
              if Set.member x $ ePatVars chi
                  then branch
                  else Branch chi $ subst c x branchExpr
    OnionSub e' s -> OnionSub (subst c x e') s
    EmptyOnion -> EmptyOnion
    LazyOp op e1 e2 -> LazyOp op (subst c x e1) (subst c x e2)
    EagerOp op e1 e2 -> EagerOp op (subst c x e1) (subst c x e2)
    Def i e1 e2 ->
      if i == x then Def i (subst c x e1) e2
                else Def i (subst c x e1) (subst c x e2)
    Assign a e1 e2 ->
      case a of
        ACell _ -> Assign a (subst c x e1) (subst c x e2)
        AIdent i ->
          let a' = if i == x then (ACell c) else a in
          Assign a' (subst c x e1) (subst c x e2)
    ExprCell _ -> e

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
