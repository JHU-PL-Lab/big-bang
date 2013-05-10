{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Language.TinyBang.Ast.WellFormedness
( IllFormedness(..)
, VarCount(..)
, checkWellFormed
, openVariables
, isClosed
) where

import Control.Applicative ((<$>), (<*>), (<*),)
import Control.Monad.RWS (RWS, evalRWS, modify, gets, get, put, tell)
import Control.Monad (unless)
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
checkWellFormed :: Expr -> Either IllFormedness VarCount
checkWellFormed = countVar

-- |Represents a variable appearance counting result.
data VarCount = VarCount
                  (Set FlowVar) -- ^ Flow variable definitions
                  (Set FlowVar) -- ^ Flow variable uses
                  (Set CellVar) -- ^ Cell variable definitions
  deriving (Eq,Ord,Show)

-- VarCount is not a monoid because union can fail.
                  
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
        else
          let valid = mconcat $ map countVar cs in
          case last cs of
            RedexDef _ _ _ -> valid
            CellGet _ _ _ -> valid
            Evaluated (ValueDef _ _ _) -> valid
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

-- | Given an expression returns the set of open variables in it.
openVariables :: Expr -> Set SomeVar
openVariables arg = snd $ evalRWS (checkClosed arg) () Set.empty

-- | Given an expression returns @True@ if and only if it has no open
--   variables.
isClosed :: Expr -> Bool
isClosed = Set.null . openVariables

class VarCloseable a where
  checkClosed :: a -> CloseM ()

-- | CloseM is the monad used in determining whether or not an expression is
--   closed.  It has three operations, which are described below.
type CloseM = RWS () (Set SomeVar) (Set SomeVar)

-- | Adds a variable to the set of variables bound in the current scope.
addVar :: IsVar v => v -> CloseM ()
addVar = modify . Set.insert . someVar

-- | Checks if a variable is bound in the current scope; if it's not, it @tell@s it.
checkVar :: IsVar v => v -> CloseM ()
checkVar x = do
  b <- gets . Set.member $ someVar x
  unless b $ tell . Set.singleton $ someVar x

-- | Performs an operation in a new scope; variables bound during the operation
--   will be unbound outside of this call.
bracket :: CloseM a -> CloseM a
bracket m = do
  frame <- get
  m <* put frame

-- | Convenience function for running @checkVar@ on two variables of possibly
--   different types.
checkVars :: (IsVar v1, IsVar v2) => v1 -> v2 -> CloseM ()
checkVars v1 v2 = checkVar v1 >> checkVar v2

-- | Convenience function for checking if a term is closed and then adding a
--   variable to the set of bound variables.
checkAdd :: (VarCloseable c, IsVar v) => c -> v -> CloseM ()
checkAdd v1 v2 = checkClosed v1 <* addVar v2

instance VarCloseable FlowVar where
  checkClosed = checkVar

instance VarCloseable CellVar where
  checkClosed = checkVar

instance VarCloseable Expr where
  checkClosed arg = case arg of
    Expr _ cs -> mapM_ checkClosed cs

instance VarCloseable Clause where
  checkClosed arg = case arg of
    RedexDef _ x r -> checkAdd r x
    CellSet _ y x -> checkVars x y
    CellGet _ x y -> checkAdd y x
    Throws _ x x' -> checkAdd x' x
    Evaluated cl -> checkClosed cl

instance VarCloseable EvaluatedClause where
  checkClosed arg = case arg of
    ValueDef _ x v -> checkAdd v x
    CellDef _ _ y x -> checkAdd x y
    Flow _ x _ x' -> checkVars x x'

instance VarCloseable Redex where
  checkClosed arg = case arg of
    Define _ x -> addVar x
    Appl _ x x' -> checkVars x x'
    BinOp _ x _ x' -> checkVars x x'

instance VarCloseable Value where
  checkClosed arg = case arg of
    VInt _ _ -> yes
    VChar _ _ -> yes
    VEmptyOnion _ -> yes
    VLabel _ _ y -> checkVar y
    VOnion _ x x' -> checkVars x x'
    VOnionFilter _ x _ _ -> checkVar x
    VScape _ pat e -> bracket $ checkClosed pat >> checkClosed e
    where yes = return ()

instance VarCloseable Pattern where
  checkClosed arg = case arg of
    ValuePattern _ y ipat -> checkAdd ipat y
    ExnPattern _ y ipat -> checkAdd ipat y

instance VarCloseable InnerPattern where
  checkClosed arg = case arg of
    PrimitivePattern _ _ -> yes
    LabelPattern _ _ y ipat -> checkAdd ipat y
    ConjunctionPattern _ ipat ipat' -> checkClosed ipat >> checkClosed ipat'
    ScapePattern _ -> yes
    EmptyOnionPattern _ -> yes
    where yes = return ()
