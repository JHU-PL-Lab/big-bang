{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

{-|
  This module implements a small-step interpreter for the TinyBang language.
-}

module Language.TinyBang.Interpreter
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

import Language.TinyBang.Ast
import Language.TinyBang.Display
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Interpreter.Compatibility
import Language.TinyBang.Interpreter.Equality
import Language.TinyBang.Interpreter.Projection

import Debug.Trace

-- |The small-step evaluation routine for TinyBang.  The result is an evaluation
--  environment (representing the heap) and a flow variable (representing the
--  result of evaluation).  If an error occurs, it is accompanied by the list
--  of unevaluated clauses when evaluation failed.
eval :: Expr -> Either (EvalError, [Clause]) (EvalEnv, FlowVar)
eval e@(Expr _ cls) =
  case checkWellFormed e of
    Left ill -> Left (IllFormedExpression ill, cls)
    Right (VarCount fvs' fvs'' cvs) ->
      let usedVars = Set.map unFlowVar (fvs' `Set.union` fvs'') `Set.union`
                     Set.map unCellVar cvs in
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
            Nothing ->
              let ill = IllFormedExpression $ InvalidExpressionEnd $ last cls in
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
  trace ("Clauses: " ++ display cls) $ case cls of
    [] -> return ()
    RedexDef orig x1 redex : _ ->
      let orig' = ComputedOrigin [orig] in
      case redex of
        Define _ x2 -> do
          v' <- flowLookup x2
          replaceFirstClause [Evaluated $ ValueDef orig' x1 v']
        Appl _ x2 x3 ->
          apply x1 x2 x3 ArgVal (left $ ApplicationFailure x2 x3) (return ())
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
                let proj = projPrim primInt
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
                replaceFirstClause [Evaluated $ ValueDef orig' x1 vnew]
              binCompareIntOp :: (Integer -> Integer -> Bool) -> EvalM ()
              binCompareIntOp opf = do
                xf <- freshFlowVar x1
                yf <- freshCellVarFromFlowVar x1
                let toBool o b =
                      let n = LabelName o $ if b then "True" else "False" in
                      return $ VLabel o n yf
                vnew <- binOp fromIntProj fromIntProj opf toBool x2 x3
                replaceFirstClause
                  [ Evaluated $ ValueDef orig' xf $ VEmptyOnion orig'
                  , Evaluated $ CellDef orig' (QualFinal orig') yf xf
                  , Evaluated $ ValueDef orig' x1 vnew ]
          in
          case op of
            OpPlus _ -> binArithIntOp (+)
            OpMinus _ -> binArithIntOp (-)
            OpLess _ -> binCompareIntOp (<)
            OpGreater _ -> binCompareIntOp (>)
            OpEqual _ -> do
              areEqual <- flowVarValueEq x2 x3
              x1f <- freshFlowVar x1
              y1f <- freshCellVarFromFlowVar x1
              let n = LabelName orig' $ if areEqual then "True" else "False"
              let vnew = VLabel orig' n y1f
              replaceFirstClause
                [ Evaluated $ ValueDef orig' x1f $ VEmptyOnion orig'
                , Evaluated $ CellDef orig' (QualFinal orig') y1f x1f
                , Evaluated $ ValueDef orig' x1 vnew ]
    CellSet _ y1 x2 : _ -> do
      st <- get
      let env = evalEnv st
      let env' = env { cellVarMap = Map.insert y1 x2 $ cellVarMap env
                     , lastVar = Nothing }
      put $ st { evalEnv = env' }
      replaceFirstClause []
    CellGet orig x1 y2 : _ -> do
      x' <- cellLookup y2
      v' <- flowLookup x'
      let orig' = ComputedOrigin [orig]
      replaceFirstClause [ Evaluated $ ValueDef orig' x1 v' ]
    cl@(Throws orig x1 x2) : cls' -> do
      env <- evalEnv <$> get
      let mx3 = Map.lookup x1 =<< Map.lookup FlowExn (flowPathMap env)
      case mx3 of
        Just x3 -> replaceFirstClause [Throws orig x3 x2]
        Nothing -> case cls' of
          RedexDef _ x4 (Appl _ x5 x1') : _ | x1' == x1 ->
            -- Application for exceptions is like application for normal values
            -- except that (1) we just keep going on failure and (2) success
            -- has to remove the "throws" as well as the application site.
            apply x4 x5 x1 ArgExn
              (setClauses $ Throws orig x4 x2 : tail cls')
              (replaceFirstClause [])
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
  where
    -- |A function containing a generalization of application semantics.
    --  This function is used by both standard application and to catch
    --  exceptions.
    apply :: FlowVar -- ^ The call site variable for the application
          -> FlowVar -- ^ The variable containing the scapes being called
          -> FlowVar -- ^ The variable containing the argument
          -> (FlowVar -> CompatibilityArgument) -- ^ The argument constructor
          -> EvalM () -- ^ The computation to use if application fails.
          -> EvalM () -- ^ A computation to be performed on success (immediately
                      --   before substituting the body).
          -> EvalM ()
    apply x1 x2 x3 constr failure success = do
      scapes <- projectAll x2 projFun
      mexpr <- applicationCompatibility (constr x3) scapes
      case mexpr of
        Just (Expr _ body) -> do
          let rbody = reverse body
          Expr _ rbody' <- freshen $ Expr (ComputedOrigin []) rbody
          cl' <- rebindLastClause (listToMaybe rbody') x1
          success
          replaceFirstClause $ reverse $ cl' : tail rbody' 
        Nothing -> failure
      where
        rebindLastClause :: Maybe Clause -> FlowVar -> EvalM Clause
        rebindLastClause mcl x =
          case mcl of
            Just (RedexDef orig _ r) -> right $ RedexDef orig x r
            Just (Evaluated (ValueDef orig _ v)) ->
              right $ Evaluated $ ValueDef orig x v
            Just cl -> left $ IllFormedExpression $ InvalidExpressionEnd cl
            Nothing -> left $ IllFormedExpression EmptyExpression

-- |A routine to freshen every variable in the provided expression.
freshen :: Expr -> EvalM Expr
freshen e =
  case checkWellFormed e of
    Left ill -> left $ IllFormedExpression ill
    Right (VarCount fvs' fvs'' cvs') -> do
      let fvs = Set.toList $ fvs' `Set.union` fvs''
      let cvs = Set.toList cvs'
      fvss <- mapM (makeSubst FlowSubstitution freshFlowVar) fvs
      cvss <- mapM (makeSubst CellSubstitution freshCellVar) cvs
      return $ substitute (collect $ fvss ++ cvss) e
  where
    makeSubst :: (a -> a -> Substitution)
              -> (a -> EvalM a) -> a -> EvalM Substitution
    makeSubst constr makeFreshVar var = flip constr var <$> makeFreshVar var
