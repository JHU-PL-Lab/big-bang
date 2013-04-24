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
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Interpreter.Compatibility
import Language.TinyBang.Interpreter.Equality
import Language.TinyBang.Interpreter.Projection

-- |The small-step evaluation routine for TinyBang.  The result is an evaluation
--  environment (representing the heap) and a flow variable (representing the
--  result of evaluation).  If an error occurs, it is accompanied by the list
--  of unevaluated clauses when evaluation failed.
eval :: Expr -> Either (EvalError, [Clause]) (EvalEnv, FlowVar)
eval e@(Expr _ cls) =
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
