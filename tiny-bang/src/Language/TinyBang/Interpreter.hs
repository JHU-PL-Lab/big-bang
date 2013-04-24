{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

{-|
  This module implements a small-step interpreter for the TinyBang language.
-}

module Language.TinyBang.Interpreter
( eval
, EvalEnv(..)
) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mappend, mempty)
import qualified Data.Set as Set
import Data.Set (Set)

import Language.TinyBang.Ast

-- |A data structure containing an evaluation environment.
data EvalEnv = EvalEnv
                { flowVarMap :: Map FlowVar Value
                  -- ^ A map describing flow variable bindings
                , cellVarMap :: Map CellVar FlowVar
                  -- ^ A map describing cell variable bindings
                , flowPathMap :: Map FlowKind (Map FlowVar FlowVar)
                  -- ^ A map indicating paths for flow
                , lastVar :: Maybe FlowVar
                  -- ^ The most recent evaluated flow variable (or Nothing if
                  --   the most recent evaluation did not define a flow
                  --   variable)
                }
                
-- |A data strcture representing evaluation state.
data EvalState = EvalState
                { evalEnv :: EvalEnv
                  -- ^ The current evaluation environment
                , evalClauses :: [Clause]
                  -- ^ The current evaluation clauses
                , evalUsedVars :: UsedVars
                  -- ^ The set of variables used so far
                }

-- |A set containing all used variables.
data UsedVars = UsedVars (Set String) Integer
                
-- |A data structure representing evaluation errors.
data EvalError
  = IllFormedExpression IllFormedness
  | FlowVarNotClosed FlowVar
  | CellVarNotClosed CellVar
  | NonFlowExpressionEnd -- TODO: report the clause which did this
  | ProjectionFailure FlowVar Projector
  -- TODO

-- |The monad in which small-step evaluation takes place.
type EvalM a = EitherT EvalError (State EvalState) a

-- |The small-step evaluation routine for TinyBang.  The result is an evaluation
--  environment (representing the heap) and a flow variable (representing the
--  result of evaluation).  If an error occurs, it is accompanied by the list
--  of unevaluated clauses when evaluation failed.
eval :: Expr -> Either (EvalError, [Clause]) (EvalEnv, FlowVar)
eval e@(Expr _ cls) = do
  case checkWellFormed e of
    Left ill -> Left (IllFormedExpression ill, cls)
    Right usedVars ->
      let initialState = EvalState
            ( EvalEnv Map.empty Map.empty Map.empty Nothing )
            cls (UsedVars usedVars 1) in
      let (merr, rstate) =
            runState (runEitherT smallStepMany) initialState in
      case merr of
        Left err -> Left (err, evalClauses rstate)
        Right () ->
          case lastVar $ evalEnv rstate of
            Just var -> Right (evalEnv rstate,var)
            Nothing -> Left (NonFlowExpressionEnd, evalClauses rstate)
  where
    smallStepMany :: EvalM ()
    smallStepMany = do
      smallStep
      cs <- evalClauses <$> get
      unless (null cs) smallStepMany
      
-- |Performs a value lookup on a given flow variable.
flowLookup :: FlowVar -> EvalM Value
flowLookup x = do
  env <- evalEnv <$> get
  let mval = Map.lookup x $ flowVarMap env
  fromMaybe (left $ FlowVarNotClosed x) $ right <$> mval

-- |Performs a variable lookup on a given cell variable.
cellLookup :: CellVar -> EvalM FlowVar
cellLookup y = do
  env <- evalEnv <$> get
  let mx = Map.lookup y $ cellVarMap env
  fromMaybe (left $ CellVarNotClosed y) $ right <$> mx

-- |Sets the clauses in a given small-step evaluation.
setClauses :: [Clause] -> EvalM ()
setClauses cls =
  modify $ \s -> EvalState (evalEnv s) cls (evalUsedVars s)

-- |Replaces the first clause of the clause list with the provided clauses.
replaceFirstClause :: [Clause] -> EvalM ()
replaceFirstClause cls =
  modify $ \s -> EvalState (evalEnv s) (cls ++ (drop 1 $ evalClauses s))
                           (evalUsedVars s)
                           
-- |Modifies the @UsedVars@ state component.
modifyUsedVars :: (UsedVars -> UsedVars) -> EvalM ()
modifyUsedVars f =
  modify $ \s -> EvalState (evalEnv s) (evalClauses s) (f $ evalUsedVars s)
  
-- |Obtains fresh variables.  This is a general form of the fresh variable
--  functions below.
freshVar :: forall a b.
            (a -> (Origin, String)) -> (Origin -> String -> Integer -> b)
         -> (b -> String)
         -> a -> EvalM b
freshVar destruct construct toString xx =
  let (origin,base) = destruct xx in findFresh origin base
  where
    findFresh :: Origin -> String -> EvalM b
    findFresh o b = do
      UsedVars u' i' <- evalUsedVars <$> get
      let xx' = construct o b i'
      if toString xx' `Set.member` u'
        then do
          modifyUsedVars (\(UsedVars u i) -> UsedVars u $ i + 1)
          findFresh o b
        else do
          modifyUsedVars (\(UsedVars u i) ->
            UsedVars (Set.insert (toString xx') u) i)
          return xx'

-- |Obtains a fresh flow variable
freshFlowVar :: FlowVar -> EvalM FlowVar
freshFlowVar = freshVar destructFlowVarToPair GenFlowVar unFlowVar

destructFlowVarToPair :: FlowVar -> (Origin, String)
destructFlowVarToPair x' = case x' of
      FlowVar o s -> (o,s)
      GenFlowVar o s _ -> (o,s)
      
-- |Obtains a fresh cell variable
freshCellVar :: CellVar -> EvalM CellVar
freshCellVar = freshVar destructCellVarToPair GenCellVar unCellVar

destructCellVarToPair :: CellVar -> (Origin, String)
destructCellVarToPair x' = case x' of
      CellVar o s -> (o,s)
      GenCellVar o s _ -> (o,s)
      
-- |Obtains a fresh cell variable based on the name of a flow variable
freshCellVarFromFlowVar :: FlowVar -> EvalM CellVar
freshCellVarFromFlowVar = freshVar destructFlowVarToPair GenCellVar unCellVar

-- |A function to perform a single small step of evaluation.  This function will
--  attempt to consume one of the clauses in the environment.  It will either
--  produce a new environment (with one of the clauses consumed or otherwise
--  processed) or an error.  If the environment has no clauses, it is returned
--  verbatim.
smallStep :: EvalM ()
smallStep = do
  cls <- evalClauses <$> get
  case cls of
    [] -> return ()
    RedexDef orig x r : _ ->
      let orig' = ComputedOrigin [orig] in
      case r of
        Define _ x' -> do
          v' <- flowLookup x'
          replaceFirstClause [Evaluated $ ValueDef orig' x v']
        Appl _ x' x'' -> undefined -- TODO
        BinOp _ x' op x'' ->
          let binOp :: (FlowVar -> EvalM (Origin, a))
                    -> (FlowVar -> EvalM (Origin, b))
                    -> (a -> b -> c)
                    -> (Origin -> c -> EvalM Value)
                    -> FlowVar -> FlowVar -> EvalM Value
              binOp fromL fromR op toV xL xR = do
                (oL,vL) <- fromL xL
                (oR,vR) <- fromR xR
                toV (ComputedOrigin [oL,oR]) $ op vL vR
              fromIntProj :: FlowVar -> EvalM (Origin, Integer)
              fromIntProj x = fromInt =<< project x (projPrim primInt)
              fromInt :: Value -> EvalM (Origin, Integer)
              fromInt v = do { let {VInt o n = v}; return (o,n) }
              toInt :: Origin -> Integer -> EvalM Value
              toInt o n = return $ VInt o n
              binArithIntOp :: (Integer -> Integer -> Integer) -> EvalM ()
              binArithIntOp opf = do
                vnew <- binOp fromIntProj fromIntProj opf toInt x' x''
                replaceFirstClause [Evaluated $ ValueDef orig' x vnew]
              binCompareIntOp :: (Integer -> Integer -> Bool) -> EvalM ()
              binCompareIntOp opf = do
                xf <- freshFlowVar x
                yf <- freshCellVarFromFlowVar x
                let toBool o b =
                      let n = LabelName o $ if b then "True" else "False" in
                      return $ VLabel o n yf
                vnew <- binOp fromIntProj fromIntProj opf toBool x' x''
                replaceFirstClause
                  [ Evaluated $ ValueDef orig' xf $ VEmptyOnion orig'
                  , Evaluated $ CellDef orig' (QualFinal orig') yf xf
                  , Evaluated $ ValueDef orig' x vnew ]
          in
          case op of
            OpPlus _ -> binArithIntOp (+)
            OpMinus _ -> binArithIntOp (-)
            OpLess _ -> binCompareIntOp (<)
            OpGreater _ -> binCompareIntOp (>)
            OpEqual _ -> do
              areEqual <- flowVarValueEq x' x''
              xf <- freshFlowVar x
              yf <- freshCellVarFromFlowVar x
              let n = LabelName orig' $ if areEqual then "True" else "False"
              let vnew = VLabel orig' n yf
              replaceFirstClause
                [ Evaluated $ ValueDef orig' xf $ VEmptyOnion orig'
                , Evaluated $ CellDef orig' (QualFinal orig') yf xf
                , Evaluated $ ValueDef orig' x vnew ]
    CellSet _ y x : _ -> do
      st <- get
      let env = evalEnv st
      let env' = env { cellVarMap = Map.insert y x $ cellVarMap env
                     , lastVar = Nothing }
      put $ st { evalEnv = env' }
      replaceFirstClause []
    CellGet orig x y : _ -> do
      x' <- cellLookup y
      v' <- flowLookup x'
      let orig' = ComputedOrigin [orig]
      replaceFirstClause [ Evaluated $ ValueDef orig' x v' ]
    cl@(Throws orig x x') : cls' -> do
      env <- evalEnv <$> get
      let mx'' = Map.lookup x =<< (Map.lookup FlowExn $ flowPathMap env)
      case mx'' of
        Just x'' -> replaceFirstClause [Throws orig x'' x']
        Nothing -> case cls' of
          RedexDef orig' x4 (Appl _ x5 x1) : cls'' | x1 == x ->
            undefined -- TODO
          _ ->
            setClauses $ cl : tail cls'
    Evaluated ecl : _ -> do
      st <- get
      let env = evalEnv st
      let env' = case ecl of
            ValueDef _ x v ->
              env { flowVarMap = Map.insert x v $ flowVarMap env
                  , lastVar = Just x }
            CellDef _ _ y x ->
              env { cellVarMap = Map.insert y x $ cellVarMap env
                  , lastVar = Nothing }
            Flow _ x k x' ->
              let fpmk = Map.findWithDefault Map.empty k (flowPathMap env) in
              let fpmk' = Map.insert x' x fpmk in
              env { flowPathMap = Map.insert k fpmk' $ flowPathMap env
                  , lastVar = Nothing } 
      put $ st { evalEnv = env' }
      replaceFirstClause []
              
-- |Performs single evaluation projection on a given variable and projector.
project :: FlowVar -> Projector -> EvalM Value
project x proj = do
  vs <- projectAll x proj
  if null vs then left $ ProjectionFailure x proj else return $ last vs

-- |Performs evaluation projection on a given variable and projector.
projectAll :: FlowVar -> Projector -> EvalM [Value]
projectAll x proj = do
  v <- flowLookup x
  case v of
    VOnion _ x' x'' -> (++) <$> projectAll x' proj <*> projectAll x'' proj
    VOnionFilter _ x' op proj' ->
      let fcond = case op of
                    OpOnionSub _ -> (== proj')  
                    OpOnionProj _ -> (/= proj')
      in if fcond proj then return [] else projectAll x' proj
    _ -> return $ if inProjector v proj then [v] else []
  where
    inProjector :: Value -> Projector -> Bool
    inProjector v proj' =
      case (v, proj') of
        (VInt _ _, ProjPrim _ (PrimInt _)) -> True
        (VChar _ _, ProjPrim _ (PrimChar _)) -> True
        (VLabel _ n _, ProjLabel _ n') | n == n' -> True
        (VScape _ _ _, ProjFun _) -> True
        _ -> False
        
-- |Calculates equality for the contents of two flow variables.
flowVarValueEq :: FlowVar -> FlowVar -> EvalM Bool
flowVarValueEq x x' = do
  FlattenedOnion p1 l1 s1 <- flowVarFlatten x
  FlattenedOnion p2 l2 s2 <- flowVarFlatten x'
  lequal <- mlequal l1 l2
  return $ (p1 == p2) && lequal && (s1 == s2)
  where
    mlequal :: Map LabelName CellVar -> Map LabelName CellVar -> EvalM Bool
    mlequal m1 m2 = do
      if Map.keysSet m1 /= Map.keysSet m2
        then return False
        else
          let cellVarPairs = Map.elems $ Map.mapWithKey f m1 in
          and <$> mapM cellVarValueEq cellVarPairs
      where
        f :: LabelName -> CellVar -> (CellVar, CellVar)
        f n c = (c, (Map.!) m2 n)
        cellVarValueEq :: (CellVar, CellVar) -> EvalM Bool
        cellVarValueEq (y,y') = do
          xn <- cellLookup y
          x'n <- cellLookup y'
          flowVarValueEq xn x'n

-- |Flattens a given flow variable into a type-indexed record.
flowVarFlatten :: FlowVar -> EvalM FlattenedOnion
flowVarFlatten x = do
  v <- flowLookup x
  case v of
    VInt _ _ -> return $ FlattenedOnion (Map.singleton primInt v) Map.empty []
    VChar _ _ -> return $ FlattenedOnion (Map.singleton primChar v) Map.empty []
    VEmptyOnion _ -> return mempty
    VLabel _ n y -> return $ FlattenedOnion Map.empty (Map.singleton n y) []
    VOnion _ x' x'' -> mappend <$> flowVarFlatten x' <*> flowVarFlatten x''
    VOnionFilter _ x' op proj -> do
      let (pkeep,lkeep,skeep) = case op of
            OpOnionProj _ -> ( primMatchProjector proj
                             , labelMatchProjector proj
                             , scapeMatchProjector proj )
            OpOnionSub _ -> ( not . primMatchProjector proj
                            , not . labelMatchProjector proj
                            , not $ scapeMatchProjector proj )
      FlattenedOnion p l s <- flowVarFlatten x'
      return $ FlattenedOnion
        (Map.filterWithKey (toss2nd pkeep) p)
        (Map.filterWithKey (toss2nd lkeep) l)
        (if skeep then s else [])
    VScape _ _ _ -> return $ FlattenedOnion Map.empty Map.empty [v]
  where
    primMatchProjector :: Projector -> PrimitiveType -> Bool
    primMatchProjector proj p = case proj of
      ProjPrim _ p' -> p == p'
      _ -> False
    labelMatchProjector :: Projector -> LabelName -> Bool
    labelMatchProjector proj n = case proj of
      ProjLabel _ n' -> n == n'
      _ -> False
    scapeMatchProjector :: Projector -> Bool
    scapeMatchProjector proj = case proj of
      ProjFun _ -> True
      _ -> False
    toss2nd :: (a -> b) -> a -> c -> b
    toss2nd f a _ = f a

-- |A data type for flattened onions.
data FlattenedOnion
  = FlattenedOnion
      (Map PrimitiveType Value) -- ^ The primitive types in the onion
      (Map LabelName CellVar)   -- ^ The labels in the onion 
      ([Value])                 -- ^ The scape values in the onion

instance Monoid FlattenedOnion where
  mempty = FlattenedOnion Map.empty Map.empty []
  mappend (FlattenedOnion p1 l1 s1) (FlattenedOnion p2 l2 s2) =
    FlattenedOnion (Map.union p2 p1) (Map.union l2 l1) (s1 ++ s2)
