{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Language.TinyBang.Ast.WellFormedness
( IllFormedness(..)
, VarCount(..)
, checkWellFormed
) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid, mappend, mconcat, mempty)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast.Data

-- |A result for the well-formedness check.
data IllFormedness
  = DuplicateFlowBinding FlowVar
  | DuplicateFlowUse FlowVar
  | DuplicateCellBinding CellVar
  | InvalidExpressionEnd Clause
  | EmptyExpression
  deriving (Eq, Ord, Show)

-- |Determines whether or not the provided expression is well-formed.  The
--  result is either an ill-formedness complaint or a set of used variable
--  names; the latter indicates that the expression is well-formed.
checkWellFormed :: Expr -> Either IllFormedness (Set String)
checkWellFormed e = do
  VarCount a b c <- countVar e
  return $ Set.unions
    [ Set.map unFlowVar a
    , Set.map unFlowVar b
    , Set.map unCellVar c ]

-- |Represents a variable appearance counting result.
data VarCount = VarCount
                  (Set FlowVar) -- ^ Flow variable definitions
                  (Set FlowVar) -- ^ Flow variable uses
                  (Set CellVar) -- ^ Cell variable definitions
  deriving (Eq,Ord,Show)
                  
-- |A monad in which variable counting takes place.  This monad contains the
--  failure modes for that counting operation.
type VarCountM a = Either IllFormedness a
                  
instance Monoid (VarCountM VarCount) where
  mempty = return $ VarCount Set.empty Set.empty Set.empty
  mappend v1 v2 = do
    (VarCount a1 a2 a3) <- v1
    (VarCount b1 b2 b3) <- v2
    VarCount
      <$> vcjoin a1 b1 DuplicateFlowBinding
      <*> vcjoin a2 b2 DuplicateFlowUse
      <*> vcjoin a3 b3 DuplicateCellBinding
    where
      vcjoin :: Ord a
             => Set a -> Set a -> (a -> IllFormedness) -> VarCountM (Set a)
      vcjoin a b f =
        let intersection = a `Set.intersection` b in
        if Set.null intersection
          then return $ a `Set.union` b
          else Left $ f $ Set.findMin intersection

-- |Creates a singleton @VarCount@ containing one defined flow variable.
singDefFlowVarCount :: FlowVar -> VarCountM VarCount
singDefFlowVarCount x = return $ VarCount (Set.singleton x) Set.empty Set.empty

-- |Creates a singleton @VarCount@ containing one used flow variable.
singUseFlowVarCount :: FlowVar -> VarCountM VarCount
singUseFlowVarCount x = return $ VarCount Set.empty (Set.singleton x) Set.empty

-- |Creates a singleton @VarCount@ containing one defined cell variable.
singDefCellVarCount :: CellVar -> VarCountM VarCount
singDefCellVarCount y = return $ VarCount Set.empty Set.empty $ Set.singleton y

-- |Calculates the variable appearances within a given construct.
class VarCountable a where
  countVar :: a -> Either IllFormedness VarCount
  
instance VarCountable Expr where
  countVar arg = case arg of
    Expr _ cs ->
      if null cs
        then Left EmptyExpression
        else case last cs of
          RedexDef _ _ (Define _ _) -> mconcat $ map countVar cs
          x -> Left $ InvalidExpressionEnd x

instance VarCountable Clause where
  countVar arg = case arg of
    RedexDef _ x r -> singDefFlowVarCount x `mappend` countVar r
    CellSet _ _ x -> singUseFlowVarCount x
    CellGet _ x _ -> singDefFlowVarCount x 
    Throws _ x x' -> singDefFlowVarCount x `mappend` singUseFlowVarCount x'
    Evaluated cl -> countVar cl

instance VarCountable EvaluatedClause where
  countVar arg = case arg of
    ValueDef _ x v -> singDefFlowVarCount x `mappend` countVar v
    CellDef _ _ y x -> singDefCellVarCount y `mappend` singUseFlowVarCount x
    Flow _ _ _ _ -> mempty

instance VarCountable Redex where
  countVar arg = case arg of
    Define _ x -> singUseFlowVarCount x
    Appl _ x x' -> singUseFlowVarCount x `mappend` singUseFlowVarCount x'
    BinOp _ x _ x' -> singUseFlowVarCount x `mappend` singUseFlowVarCount x'

instance VarCountable Value where
  countVar arg = case arg of
    VInt _ _ -> mempty
    VChar _ _ -> mempty
    VEmptyOnion _ -> mempty
    VLabel _ _ _ -> mempty
    VOnion _ x x' -> singUseFlowVarCount x `mappend` singUseFlowVarCount x'
    VOnionFilter _ x _ _ -> singUseFlowVarCount x
    VScape _ pat e -> countVar pat `mappend` countVar e

instance VarCountable Pattern where
  countVar arg = case arg of
    ValuePattern _ y ipat -> singDefCellVarCount y `mappend` countVar ipat
    ExnPattern _ y ipat -> singDefCellVarCount y `mappend` countVar ipat

instance VarCountable InnerPattern where
  countVar arg = case arg of
    PrimitivePattern _ _ -> mempty
    LabelPattern _ _ y ipat -> singDefCellVarCount y `mappend` countVar ipat
    ConjunctionPattern _ ipat ipat' -> countVar ipat `mappend` countVar ipat'
    ScapePattern _ -> mempty
    EmptyOnionPattern _ -> mempty
