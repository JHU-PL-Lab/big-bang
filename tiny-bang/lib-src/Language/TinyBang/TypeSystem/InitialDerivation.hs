{-# LANGUAGE ScopedTypeVariables #-}

{-|
  This module provides a routine for determining the initial constraint database
  for an expression.
-}
module Language.TinyBang.TypeSystem.InitialDerivation
where -- TODO
{-
( InitialDerivationError(..)
, initialDerivation
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast as A
import Language.TinyBang.Display hiding (empty)
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types as T

-- |A datatype for errors which may occur during initial derivation.
data InitialDerivationError
  = IllFormedExpression IllFormedness
  | OpenExpression (Set AnyVar)
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
  let history = DerivedFromSource $ ClauseElement clause in
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
    CellSet _ y x ->
      let a = derivFlowVar x in
      let b = derivCellVar y in
      let constraint = cwrap $ CellSettingConstraint a b in
      return (Nothing, singleton constraint history)
    CellGet _ x y ->
      let a = derivFlowVar x in
      let b = derivCellVar y in
      let constraint = cwrap $ CellLoadingConstraint b a in
      return (Just a, singleton constraint history)
    Throws _ x1 x2 ->
      let a1 = derivFlowVar x1 in
      let a2 = derivFlowVar x2 in
      let constraint = cwrap $ ExceptionConstraint a2 a1 in
      return (Just a1, singleton constraint history)
    Evaluated ecl -> case ecl of
      ValueDef _ x1 v -> do
        let a1 = derivFlowVar x1
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
                let tpat = patternDerivation pat
                (tvar, db::db) <- expressionDerivation expr
                return $ Scape tpat tvar db
        let constraint = WrapTypeConstraint $ TypeConstraint t a1
        return (Just a1, singleton constraint history)
      CellDef _ q y x ->
        let a = derivFlowVar x in
        let b = derivCellVar y in
        let qhist = DerivedFromSource $ QualifierElement q in
        let qdb =  case q of
                    QualFinal _ -> singleton
                        (cwrap $ FinalConstraint b) qhist
                    QualImmutable _ -> singleton
                        (cwrap $ ImmutableConstraint b) qhist
                    QualNone _ -> empty
        in
        let constraint = cwrap $
                            CellCreationConstraint a b in
        return (Nothing, singleton constraint history `union` qdb)
      Flow _ x1 k x2 ->
        let a1 = derivFlowVar x1 in
        let a2 = derivFlowVar x2 in
        let constraint = cwrap $ FlowConstraint a1 k a2 in
        return (Nothing, singleton constraint history)
      
-- |Performs initial derivation for patterns.
patternDerivation :: Pattern -> PatternType
patternDerivation pat = case pat of
  A.ValuePattern _ y ipat -> f y ipat T.ValuePattern
  A.ExnPattern _ y ipat -> f y ipat T.ExnPattern
  where
    f y ipat constr = constr (derivCellVar y) $ innerPatternDerivation ipat
  
-- |Performs initial derivation for inner patterns.
innerPatternDerivation :: InnerPattern -> InnerPatternType
innerPatternDerivation ipat = case ipat of
  A.PrimitivePattern _ p -> T.PrimitivePattern p
  A.LabelPattern _ n y ipat' ->
    let b = derivCellVar y in
    let tipat' = innerPatternDerivation ipat' in
    T.LabelPattern n b tipat'
  A.ConjunctionPattern _ ipat' ipat'' ->
    let tipat' = innerPatternDerivation ipat' in
    let tipat'' = innerPatternDerivation ipat'' in
    T.ConjunctivePattern tipat' tipat''
  A.ScapePattern _ -> T.ScapePattern
  A.EmptyOnionPattern _ -> T.EmptyOnionPattern

-- |Creates a type variable for the specified flow variable.
derivFlowVar :: FlowVar -> FlowTVar
derivFlowVar x = FlowTVar x noContour

-- |Creates a type variable for the specified cell variable.
derivCellVar :: CellVar -> CellTVar
derivCellVar y = CellTVar y noContour
-}
