{-# LANGUAGE ScopedTypeVariables #-}

{-|
  This module provides a routine for determining the initial constraint database
  for an expression.
-}
module Language.TinyBang.TypeSystem.TypeInference.InitialDerivation
( InitialDerivationError(..)
, initialDerivation
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Types

import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.TypeSystem.Types as T

-- |A datatype for errors which may occur during initial derivation.
data InitialDerivationError
  = IllFormedExpression IllFormedness
  | NotClosed SomeVar

-- |Derives the initial constraint database for a given expression. 
initialDerivation :: (ConstraintDatabase db)
                  => Expr -> Either InitialDerivationError db
initialDerivation expr = do
  _ <- either (Left . IllFormedExpression) Right $ checkWellFormed expr
  (_, db) <- expressionDerivation emptyContext expr
  return db

-- |Represents the type for initial derivation computations.
type DerivM a = Either InitialDerivationError a

-- |Represents the type of type contexts.
data Context = Context
                { flowContext :: Map FlowVar FlowTVar
                , cellContext :: Map CellVar CellTVar }

-- |Performs expression derivation.
expressionDerivation :: forall db. (ConstraintDatabase db)
                     => Context -> Expr -> DerivM (FlowTVar, db)
expressionDerivation context (Expr _ cls) =
  clauseListDerivation context cls

-- |Performs derivation on a clause list.  This is the meat of expression
--  derivation, but is separated for the implementation.
clauseListDerivation :: forall db. (ConstraintDatabase db)
                     => Context -> [Clause] -> DerivM (FlowTVar, db)
clauseListDerivation context cls =
  case cls of
    [] -> error $ "Discovered empty expression but well-formedness check "
                    ++ "should have excluded it!"
    cl:cls' -> do
      (context', mtvar, db::db) <- clauseDerivation context cl
      case cls' of
        [] -> case mtvar of
          Just a@(FlowTVar _ Nothing) ->
            return (a, db)
          _ -> error $
            "Expression's last clause derived with bad type variable ("
            ++ show mtvar ++ "); this should have been prohibited by "
            ++ "well-formedness!"
        _ -> do
          (tvar, db'::db) <- clauseListDerivation
                                (context `contextMerge` context') cls'
          return (tvar, db' `union` db)

-- |Performs initial derivation for clauses.
clauseDerivation :: (ConstraintDatabase db)
                 => Context -> Clause -> DerivM (Context, Maybe FlowTVar, db)
clauseDerivation context clause =
  let history = DerivedFromSource $ ClauseElement clause in
  case clause of
    RedexDef _ x1 r ->
      let a1 = derivFlowVar x1 in
      let newContext = singFlowContext x1 a1 in
      let constraint = case r of
            Define _ x2 ->
              let a2 = derivFlowVar x2 in
              WrapIntermediateConstraint $ IntermediateConstraint a2 a1
            Appl _ x2 x3 ->
              let a2 = derivFlowVar x2 in
              let a3 = derivFlowVar x3 in
              WrapApplicationConstraint $ ApplicationConstraint a2 a3 a1
            BinOp _ x2 op x3 ->
              let a2 = derivFlowVar x2 in
              let a3 = derivFlowVar x3 in
              WrapOperationConstraint $ OperationConstraint a2 op a3 a1              
      in return (newContext, Just a1, singleton constraint history)
    CellSet _ y x ->
      let a = derivFlowVar x in
      let b = derivCellVar y in
      let constraint = WrapCellSettingConstraint $ CellSettingConstraint a b in
      return (emptyContext, Nothing, singleton constraint history)
    CellGet _ x y ->
      let a = derivFlowVar x in
      let b = derivCellVar y in
      let constraint = WrapCellLoadingConstraint $ CellLoadingConstraint b a in
      return (emptyContext, Nothing, singleton constraint history)
    Throws _ x1 x2 ->
      let a1 = derivFlowVar x1 in
      let a2 = derivFlowVar x2 in
      let constraint = WrapExceptionConstraint $ ExceptionConstraint a2 a1 in
      return (emptyContext, Just a1, singleton constraint history)
    Evaluated ecl -> case ecl of
      ValueDef _ x1 v -> do
        let a1 = derivFlowVar x1
        let newContext = singFlowContext x1 a1
        t <- case v of
              VInt _ _ -> return $ Primitive primInt
              VChar _ _ -> return $ Primitive primChar
              VEmptyOnion _ -> return EmptyOnion
              VLabel _ n y -> return $ Label n $ derivCellVar y
              VOnion _ x2 x3 ->
                let a2 = derivFlowVar x2 in
                let a3 = derivFlowVar x3 in
                return $ Onion a2 a3
              VOnionFilter _ x2 op proj ->
                let a2 = derivFlowVar x2 in
                return $ OnionFilter a2 op proj
              VScape _ pat expr -> do
                let (context', tpat) = patternDerivation context pat
                (tvar, db::db) <- expressionDerivation
                                    (context `contextMerge` context')
                                    expr
                return $ Scape tpat tvar db
        let constraint = WrapTypeConstraint $ TypeConstraint t a1
        return (newContext, Just a1, singleton constraint history)
      CellDef _ q y x ->
        let a = derivFlowVar x in
        let b = derivCellVar y in
        let qhist = DerivedFromSource $ QualifierElement q in
        let qdb =  case q of
                    QualFinal _ -> singleton
                        (WrapFinalConstraint $ FinalConstraint b) qhist
                    QualImmutable _ -> singleton
                        (WrapImmutableConstraint $ ImmutableConstraint b) qhist
                    QualNone _ -> empty
        in
        let newContext = singCellContext y b in
        let constraint = WrapCellCreationConstraint $
                            CellCreationConstraint a b in
        return (newContext, Nothing, singleton constraint history `union` qdb)
      Flow _ x1 k x2 ->
        let a1 = derivFlowVar x1 in
        let a2 = derivFlowVar x2 in
        let constraint = WrapFlowConstraint $ FlowConstraint a1 k a2 in
        return (emptyContext, Nothing, singleton constraint history)
      
-- |Performs initial derivation for patterns.
patternDerivation :: Context -> Pattern -> (Context, PatternType)
patternDerivation context pat = case pat of
  A.ValuePattern _ y ipat -> f y ipat T.ValuePattern
  A.ExnPattern _ y ipat -> f y ipat T.ExnPattern
  where
    f y ipat constr =
      let (context', tipat) = innerPatternDerivation context ipat in
      let b = derivCellVar y in
      (context' `contextMerge` singCellContext y b, constr b tipat)
  
-- |Performs initial derivation for inner patterns.
innerPatternDerivation :: Context -> InnerPattern -> (Context, InnerPatternType)
innerPatternDerivation context ipat = case ipat of
  A.PrimitivePattern _ p -> (emptyContext, T.PrimitivePattern p)
  A.LabelPattern _ n y ipat' ->
    let b = derivCellVar y in
    let (context', tipat') = innerPatternDerivation context ipat' in
    (context' `contextMerge` context, T.LabelPattern n b tipat')
  A.ConjunctionPattern _ ipat' ipat'' ->
    let (context', tipat') = innerPatternDerivation context ipat' in
    let (context'', tipat'') = innerPatternDerivation context ipat'' in
    (context' `contextMerge` context'', T.ConjunctivePattern tipat' tipat'')
  A.ScapePattern _ -> (emptyContext, T.ScapePattern)
  A.EmptyOnionPattern _ -> (emptyContext, T.EmptyOnionPattern)

-- |Merges two contexts.
contextMerge :: Context -> Context -> Context
contextMerge context context' =
  -- NOTE: This relies on the assumption that the contexts are derived from
  -- well-formed expressions and are therefore disjoint.
  Context
    { flowContext = flowContext context `Map.union` flowContext context'
    , cellContext = cellContext context `Map.union` cellContext context' }

-- |Creates a type variable for the specified flow variable.
derivFlowVar :: FlowVar -> FlowTVar
derivFlowVar x = FlowTVar x Nothing

-- |Creates a type variable for the specified cell variable.
derivCellVar :: CellVar -> CellTVar
derivCellVar y = CellTVar y Nothing

-- |Creates a singleton flow context.
singFlowContext :: FlowVar -> FlowTVar -> Context
singFlowContext x a = Context
  { flowContext = Map.singleton x a
  , cellContext = Map.empty }

-- |Creates a singleton flow context.
singCellContext :: CellVar -> CellTVar -> Context
singCellContext y b = Context
  { flowContext = Map.empty
  , cellContext = Map.singleton y b }

-- |An empty context value.
emptyContext :: Context
emptyContext = Context { flowContext = Map.empty, cellContext = Map.empty }
