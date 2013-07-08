{-# LANGUAGE TupleSections, ScopedTypeVariables, TemplateHaskell #-}

{-|
  This module implements a small-step interpreter for the PatBang language.
-}

module Language.PatBang.Interpreter
( eval
-- re-exports
, EvalEnv(..)
, EvalError(..)
, EvalState(..)
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Either
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

import Language.PatBang.Ast
import Language.PatBang.Interpreter.Basis
import Language.PatBang.Interpreter.Compatibility
import Language.PatBang.Interpreter.Equality
import Language.PatBang.Interpreter.Projection
import Language.PatBang.Interpreter.Variables
import Language.PatBang.Logging

$(loggingFunctions)

-- |The small-step evaluation routine for PatBang.  The result is an evaluation
--  environment (representing the heap) and a flow variable (representing the
--  result of evaluation).  If an error occurs, it is accompanied by the list
--  of unevaluated clauses when evaluation failed.
eval :: Expr -> Either (EvalError, [Clause]) (EvalEnv, FlowVar)
eval e@(Expr _ cls) =
  let openVars = openVariables e in
  if not $ Set.null openVars
    then Left $ (OpenExpression openVars, cls)
    else
      case checkWellFormed e of
        Left ill -> Left (IllFormedExpression ill, cls)
        Right (VarCount fvs') ->
          let usedVars = Set.map unFlowVar fvs' in
          let initialState = EvalState
                ( EvalEnv Map.empty Nothing )
                cls (UsedVars usedVars 1) in
          let (merr, rstate) =
                runState (runEitherT smallStepMany) initialState in
          case merr of
            Left err -> Left (err, evalClauses rstate)
            Right () ->
              case lastVar $ evalEnv rstate of
                Just var -> Right (evalEnv rstate,var)
                Nothing ->
                  let ill = IllFormedExpression EmptyExpression in
                  Left (ill, evalClauses rstate)
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
  _infoVal "Clauses: " cls
  case cls of
    [] -> return ()
    RedexDef orig x1 redex : _ ->
      let orig' = ComputedOrigin [orig] in
      case redex of
        Define _ x2 -> do
          v' <- flowLookup x2
          replaceFirstClause [ValueDef orig' x1 v']
        Appl _ x2 x3 ->
          apply x1 x2 x3 (left $ ApplicationFailure x2 x3) (return ())
        BinOp _ x2 op x3 ->
          let binOp :: (FlowVar -> EvalM (Origin, a))
                    -> (FlowVar -> EvalM (Origin, b))
                    -> (a -> b -> c)
                    -> (Origin -> c -> EvalM Value)
                    -> FlowVar -> FlowVar -> EvalM Value
              binOp fromL fromR oper toV xL xR = do
                (oL,vL) <- fromL xL
                (oR,vR) <- fromR xR
                toV (ComputedOrigin [oL,oR]) $ oper vL vR
              fromIntProj :: FlowVar -> EvalM (Origin, Integer)
              fromIntProj x = do
                let proj = anyProjPrim primInt
                mv <- project x proj
                case mv of
                  Just v -> fromInt v
                  Nothing -> left $ ProjectionFailure x proj
              fromInt :: Value -> EvalM (Origin, Integer)
              fromInt v = do { let {VInt o n = v}; return (o,n) }
              toInt :: Origin -> Integer -> EvalM Value
              toInt o n = return $ VInt o n
              binArithIntOp :: (Integer -> Integer -> Integer) -> EvalM ()
              binArithIntOp opf = do
                vnew <- binOp fromIntProj fromIntProj opf toInt x2 x3
                replaceFirstClause [ValueDef orig' x1 vnew]
              binCompareIntOp :: (Integer -> Integer -> Bool) -> EvalM ()
              binCompareIntOp opf = do
                xf <- freshFlowVar x1
                let toBool o b =
                      let n = LabelName o $ if b then "True" else "False" in
                      return $ VLabel o n xf
                vnew <- binOp fromIntProj fromIntProj opf toBool x2 x3
                replaceFirstClause
                  [ ValueDef orig' xf $ VEmptyOnion orig'
                  , ValueDef orig' x1 vnew ]
          in
          case op of
            OpPlus _ -> binArithIntOp (+)
            OpMinus _ -> binArithIntOp (-)
            OpLess _ -> binCompareIntOp (<)
            OpGreater _ -> binCompareIntOp (>)
            OpEqual _ -> do
              areEqual <- flowVarValueEq x2 x3
              x1f <- freshFlowVar x1
              let n = LabelName orig' $ if areEqual then "True" else "False"
              let vnew = VLabel orig' n x1f
              replaceFirstClause
                [ ValueDef orig' x1f $ VEmptyOnion orig'
                , ValueDef orig' x1 vnew ]
    ValueDef _ x v : _ -> do
      st <- get
      let env = evalEnv st
      let env' = env { flowVarMap = Map.insert x v $ flowVarMap env
                     , lastVar = Just x }
      put $ st { evalEnv = env' }
      replaceFirstClause []
  where
    -- |A function containing a generalization of application semantics.
    --  This function is used by both standard application and to catch
    --  exceptions.
    apply :: FlowVar -- ^ The call site variable for the application
          -> FlowVar -- ^ The variable containing the scapes being called
          -> FlowVar -- ^ The variable containing the argument
          -> EvalM () -- ^ The computation to use if application fails.
          -> EvalM () -- ^ A computation to be performed on success (immediately
                      --   before substituting the body).
          -> EvalM ()
    apply x1 x2 x3 failure success = do
      scapes <- projectAll x2 anyProjFun
      mexpr <- applicationCompatibility x3 scapes
      case mexpr of
        Just (Expr _ body) -> do
          Expr _ body' <- freshen $ Expr (ComputedOrigin []) body
          let rbody' = reverse body'
          cl' <- rebindLastClause (listToMaybe rbody') x1
          success
          replaceFirstClause $ reverse $ cl' : tail rbody' 
        Nothing -> failure
      where
        rebindLastClause :: Maybe Clause -> FlowVar -> EvalM Clause
        rebindLastClause mcl x =
          case mcl of
            Just (RedexDef orig _ r) -> right $ RedexDef orig x r
            Just (ValueDef orig _ v) ->
              right $ ValueDef orig x v
            Nothing -> left $ IllFormedExpression EmptyExpression

