{-# LANGUAGE ScopedTypeVariables #-}

{-|
  This module provides a routine for determining the initial constraint database
  for an expression.
-}
module Language.TinyBang.TypeSystem.InitialDerivation
( InitialDerivationError(..)
, initialDerivation
) where

import Control.Applicative
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.Display

-- |A datatype for errors which may occur during initial derivation.
data InitialDerivationError
  = IllFormedExpression (Set IllFormedness)
  deriving (Eq, Ord, Show)

instance Display InitialDerivationError where
  makeDoc e = case e of
    IllFormedExpression ills -> makeDoc ills

-- |Derives the initial constraint database for a given expression. 
initialDerivation :: (ConstraintDatabase db)
                  => Expr -> Either InitialDerivationError db
initialDerivation expr = do
  let ills = checkWellFormed expr
  unless (Set.null ills) $ Left $ IllFormedExpression ills
  snd <$> expressionDerivation expr

-- |Represents the type for initial derivation computations.
type DerivM a = Either InitialDerivationError a

-- |Performs expression derivation.
expressionDerivation :: forall db. (ConstraintDatabase db)
                     => Expr -> DerivM (TVar, db)
expressionDerivation (Expr o cls) =
  _derivationOverListOfClauses EmptyExpression clauseDerivation o cls  

-- |Performs initial derivation for clauses.
clauseDerivation :: forall db. (ConstraintDatabase db)
                 => Clause -> DerivM (TVar, Constraint db)
clauseDerivation clause =
  let h = DerivedFromSource $ ClauseElement clause in
  case clause of
    RedexDef _ x1 r -> do
      let a1 = derivVar x1
      constraint <- case r of
            Define _ x2 -> do
              let a2 = derivVar x2
              return $ a2 <: a1 .: h
            Appl _ x2 x3 -> do
              let a2 = derivVar x2
              let a3 = derivVar x3
              return $ (a2,a3) <: a1 .: h
      return (a1, constraint)
    Evaluated ecl -> case ecl of
      ValueDef _ x1 v -> do
        let a1 = derivVar x1
        t :: Type db <- case v of
              VPrimitive _ pv ->
                case pv of
                  VInt _ _ -> return $ TPrimitive PrimInt
              VEmptyOnion _ -> return TEmptyOnion
              VLabel _ n x -> return $ TLabel n $ mktov $ derivVar x
              VOnion _ x2 x3 -> do
                let a2 = derivVar x2
                let a3 = derivVar x3
                return $ TOnion (mktov a2) (mktov a3)
              VScape _ pat expr -> do
                (tvarP, csP :: db) <- patternDerivation pat
                (tvarE, csE :: db) <- expressionDerivation expr
                return $ TScape tvarP csP tvarE csE
        return (a1, t <: a1 .: h)

-- |Performs pattern derivation.
patternDerivation :: (ConstraintDatabase db)
                  => Pattern -> DerivM (TVar, db)
patternDerivation (Pattern o pcls) =
  _derivationOverListOfClauses EmptyPattern patternClauseDerivation o pcls

-- |Performs initial derivation for pattern clauses.
patternClauseDerivation :: forall db. (ConstraintDatabase db)
                        => PatternClause -> DerivM (TVar, Constraint db)
patternClauseDerivation pcl =
  let h = DerivedFromSource $ PatternElement pcl in
  case pcl of
    PatternClause _ x pv ->
      return $
        let t :: Type db = patternValueDerivation pv in
        let a = derivVar x in
        (a, t <: a .: h)
    
-- |Performs initial derivation for a pattern value.
patternValueDerivation :: (ConstraintDatabase db)
                       => PatternValue -> Type db
patternValueDerivation pv =
  case pv of
    PPrimitive _ pt -> TPrimitive pt
    PEmptyOnion _ -> TEmptyOnion
    PLabel _ n x -> TLabel n $ mktov $ derivVar x
    PConjunction _ x1 x2 ->
      let a1 = derivVar x1 in
      let a2 = derivVar x2 in
      TOnion (mktov a1) (mktov a2)
  
-- |Creates a type variable for the specified flow variable.
derivVar :: Var -> TVar
derivVar x = TVar x noContour

-- Common utilities

_derivationOverListOfClauses :: forall a db. (ConstraintDatabase db)
                             => (Origin -> IllFormedness)
                             -> (a -> DerivM (TVar, Constraint db))
                             -> Origin
                             -> [a]
                             -> DerivM (TVar, db)
_derivationOverListOfClauses emptyIllFormednessFn clauseDerivationFn o clauses =
  if null clauses
    then Left $ IllFormedExpression $ Set.singleton $ emptyIllFormednessFn o
    else do
      (vars, csLst) <- unzip <$> mapM clauseDerivationFn clauses
      return (last vars, fromList csLst)
