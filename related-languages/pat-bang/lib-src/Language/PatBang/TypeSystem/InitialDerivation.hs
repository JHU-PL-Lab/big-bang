{-# LANGUAGE ScopedTypeVariables #-}

{-|
  This module provides a routine for determining the initial constraint database
  for an expression.
-}
module Language.PatBang.TypeSystem.InitialDerivation
( InitialDerivationError(..)
, initialDerivation
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast as A
import Language.PatBang.Display hiding (empty)
import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Contours
import Language.PatBang.TypeSystem.Types as T

-- |A datatype for errors which may occur during initial derivation.
data InitialDerivationError
  = IllFormedExpression IllFormedness
  | OpenExpression (Set FlowVar)
  deriving (Eq, Ord, Show)
instance Display InitialDerivationError where
  makeDoc err = case err of
    IllFormedExpression ill -> text "IllFormedExpression" <+> makeDoc ill
    OpenExpression vs -> text "OpenExpression" <+> makeDoc vs

-- |Derives the initial constraint database for a given expression. 
initialDerivation :: (ConstraintDatabase db)
                  => Expr -> Either InitialDerivationError db
initialDerivation expr = do
  _ <- either (Left . IllFormedExpression) Right $ checkWellFormed expr
  let vs = openVariables expr
  () <- if Set.null vs then return () else Left $ OpenExpression vs
  (_, db) <- expressionDerivation expr
  return db

-- |Represents the type for initial derivation computations.
type DerivM a = Either InitialDerivationError a

-- |Performs expression derivation.
expressionDerivation :: forall db. (ConstraintDatabase db)
                     => Expr -> DerivM (FlowTVar, db)
expressionDerivation (Expr _ cls) = clauseListDerivation cls

-- |Performs derivation on a clause list.  This is the meat of expression
--  derivation, but is separated for the implementation.
clauseListDerivation :: forall db. (ConstraintDatabase db)
                     => [Clause] -> DerivM (FlowTVar, db)
clauseListDerivation cls =
  case cls of
    [] -> error $ "Discovered empty expression but well-formedness check "
                    ++ "should have excluded it!"
    cl:cls' -> do
      (mtvar, db::db) <- clauseDerivation cl
      case cls' of
        [] -> case mtvar of
          Just a@(FlowTVar _ (PossibleContour Nothing)) ->
            return (a, db)
          _ -> error $
            "Expression's last clause derived with bad type variable ("
            ++ show mtvar ++ "); this should have been prohibited by "
            ++ "well-formedness!"
        _ -> do
          (tvar, db'::db) <- clauseListDerivation cls'
          return (tvar, db' `union` db)

-- |Performs initial derivation for clauses.
clauseDerivation :: (ConstraintDatabase db)
                 => Clause -> DerivM (Maybe FlowTVar, db)
clauseDerivation clause =
  let history = DerivedFromSource clause in
  case clause of
    RedexDef _ x1 r ->
      let a1 = derivFlowVar x1 in
      let constraint = case r of
            Define _ x2 ->
              let a2 = derivFlowVar x2 in
              cwrap $ IntermediateConstraint a2 a1
            Appl _ x2 x3 ->
              let a2 = derivFlowVar x2 in
              let a3 = derivFlowVar x3 in
              cwrap $ ApplicationConstraint a2 a3 a1
            BinOp _ x2 op x3 ->
              let a2 = derivFlowVar x2 in
              let a3 = derivFlowVar x3 in
              cwrap $ OperationConstraint a2 op a3 a1              
      in return (Just a1, singleton constraint history)
    ValueDef _ x1 v -> do
      let a1 = derivFlowVar x1
      t <- case v of
            VInt _ _ -> return $ Primitive primInt
            VEmptyOnion _ -> return EmptyOnion
            VLabel _ n x -> return $ Label n $ derivFlowVar x
            VOnion _ x2 x3 ->
              let a2 = derivFlowVar x2 in
              let a3 = derivFlowVar x3 in
              return $ Onion a2 a3
            VFunction _ (AstList _ xs) e -> do
              let aa = map derivFlowVar xs
              (tvar, db::db) <- expressionDerivation e
              return $ Function aa tvar db
            VPattern _ (AstList _ ys) p ->
              return $ Pattern (map derivPatVar ys) (patternDerivation p)
            VScape _ x2 x3 ->
              return $ Scape (derivFlowVar x2) (derivFlowVar x3)
      let constraint = WrapTypeConstraint $ TypeConstraint t a1
      return (Just a1, singleton constraint history)
      
-- |Performs initial derivation for patterns.
patternDerivation :: A.PatternBody -> T.PatternBody
patternDerivation pat = case pat of
  A.PPrim _ p -> T.PPrim p
  A.PLabel _ n pat' -> T.PLabel n $ patternDerivation pat'
  A.PFun _ -> T.PFun
  A.PPat _ -> T.PPat
  A.PScape _ -> T.PScape
  A.PConj _ pat1 pat2 ->
    T.PConj (patternDerivation pat1) (patternDerivation pat2)
  A.PSubst _ x (AstList _ pats) ->
    T.PSubst (derivFlowVar x) (map patternDerivation pats)
  A.PRec _ y pat' -> T.PRec (derivPatVar y) (patternDerivation pat')
  A.PVar _ y -> T.PVar $ derivPatVar y

-- |Creates a type variable for the specified flow variable.
derivFlowVar :: FlowVar -> FlowTVar
derivFlowVar x = FlowTVar x noContour

-- |Creates a type variable for the specified cell variable.
derivPatVar :: PatVar -> PatTVar
derivPatVar y = PatTVar y
