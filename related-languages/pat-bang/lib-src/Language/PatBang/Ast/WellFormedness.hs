{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Language.PatBang.Ast.WellFormedness
( IllFormedness(..)
, VarCount(..)
, checkWellFormed
, openVariables
, isClosed
) where

import Control.Applicative ((<$>), (<*))
import Control.Monad.RWS (RWS, evalRWS, modify, gets, get, put, tell)
import Control.Monad (unless)
import Data.Monoid (Monoid, mappend, mconcat, mempty)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast.Data
import Language.PatBang.Display

-- |A result for the well-formedness check.
data IllFormedness
  = DuplicateFlowBinding FlowVar
  | EmptyExpression
  deriving (Eq, Ord, Show)
instance Display IllFormedness where makeDoc = text . show

-- |Determines whether or not the provided expression is well-formed.  The
--  result is either an ill-formedness complaint or a set of used variable
--  names; the latter indicates that the expression is well-formed.
checkWellFormed :: Expr -> Either IllFormedness VarCount
checkWellFormed = countVar

-- |Represents a variable appearance counting result.
data VarCount = VarCount
                  (Set FlowVar) -- ^ Flow variable definitions
  deriving (Eq,Ord,Show)

-- VarCount is not a monoid because union can fail.
                  
-- |A monad in which variable counting takes place.  This monad contains the
--  failure modes for that counting operation.
type VarCountM a = Either IllFormedness a
                  
instance Monoid (VarCountM VarCount) where
  mempty = return $ VarCount Set.empty
  mappend v1 v2 = do
    (VarCount a1) <- v1
    (VarCount b1) <- v2
    VarCount <$> vcjoin a1 b1 DuplicateFlowBinding
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
singDefFlowVarCount x = return $ VarCount (Set.singleton x)

-- |Calculates the variable appearances within a given construct.
class VarCountable a where
  countVar :: a -> Either IllFormedness VarCount
  
instance (VarCountable a) => VarCountable [a] where
  countVar arg = mconcat $ map countVar arg
  
instance VarCountable Expr where
  countVar arg = case arg of
    Expr _ cs ->
      if null cs
        then Left EmptyExpression
        else mconcat $ map countVar cs

instance VarCountable Clause where
  countVar arg = case arg of
    RedexDef _ x r -> singDefFlowVarCount x `mappend` countVar r
    ValueDef _ x v -> singDefFlowVarCount x `mappend` countVar v

instance VarCountable Redex where
  countVar _ = mempty
  
instance VarCountable a => VarCountable (AstList a) where
  countVar arg = case arg of
    AstList _ els -> countVar els

instance VarCountable Value where
  countVar arg = case arg of
    VInt _ _ -> mempty
    VEmptyOnion _ -> mempty
    VLabel _ _ _ -> mempty
    VOnion _ _ _ -> mempty
    VFunction _ (AstList _ xs) e ->
      return (VarCount (Set.fromList xs)) `mappend` countVar e
    VPattern _ _ _ -> mempty
    VScape _ _ _ -> mempty

-- | Given an expression returns the set of open variables in it.
openVariables :: Expr -> Set FlowVar
openVariables arg = snd $ evalRWS (checkClosed arg) () Set.empty

-- | Given an expression returns @True@ if and only if it has no open
--   variables.
isClosed :: Expr -> Bool
isClosed = Set.null . openVariables

class VarCloseable a where
  checkClosed :: a -> CloseM ()

-- | CloseM is the monad used in determining whether or not an expression is
--   closed.  It has three operations, which are described below.
type CloseM = RWS () (Set FlowVar) (Set FlowVar)

-- | Adds a variable to the set of variables bound in the current scope.
addVar :: FlowVar -> CloseM ()
addVar = modify . Set.insert

-- | Adds multiple variables to the set of variables bound in current scope.
addVars :: [FlowVar] -> CloseM ()
addVars = modify . Set.union . Set.fromList

-- | Checks if a variable is bound in the current scope; if it's not, it @tell@s it.
checkVar :: FlowVar -> CloseM ()
checkVar x = do
  b <- gets $ Set.member x
  unless b $ tell $ Set.singleton x

-- | Performs an operation in a new scope; variables bound during the operation
--   will be unbound outside of this call.
bracket :: CloseM a -> CloseM a
bracket m = do
  frame <- get
  m <* put frame

-- | Convenience function for running @checkVar@ on two variables of possibly
--   different types.
checkVars :: FlowVar -> FlowVar -> CloseM ()
checkVars v1 v2 = checkVar v1 >> checkVar v2

-- | Convenience function for checking if a term is closed and then adding a
--   variable to the set of bound variables.
checkAdd :: (VarCloseable c) => c -> FlowVar -> CloseM ()
checkAdd v1 v2 = checkClosed v1 <* addVar v2

-- | Convenience function for checking if a term is closed and then adding a
--   variable to the set of bound variables.
checkAdds :: (VarCloseable c) => c -> [FlowVar] -> CloseM ()
checkAdds v1 vs2 = checkClosed v1 <* addVars vs2

instance (VarCloseable a) => VarCloseable [a] where
  checkClosed = mapM_ checkClosed

instance VarCloseable FlowVar where
  checkClosed = checkVar

instance VarCloseable Expr where
  checkClosed arg = case arg of
    Expr _ cs -> mapM_ checkClosed cs

instance VarCloseable Clause where
  checkClosed arg = case arg of
    RedexDef _ x r -> checkAdd r x
    ValueDef _ x v -> checkAdd v x

instance VarCloseable Redex where
  checkClosed arg = case arg of
    Define _ x -> addVar x
    Appl _ x x' -> checkVars x x'
    BinOp _ x _ x' -> checkVars x x'

instance VarCloseable Value where
  checkClosed arg = case arg of
    VInt _ _ -> yes
    VEmptyOnion _ -> yes
    VLabel _ _ x -> checkVar x
    VOnion _ x x' -> checkVars x x'
    VFunction _ (AstList _ xs) e -> checkAdds e xs
    VPattern _ _ p -> checkClosed p
    VScape _ pat e -> bracket $ checkClosed pat >> checkClosed e
    where yes = return ()

instance VarCloseable PatternBody where
  checkClosed arg = case arg of
    PPrim _ _ -> yes
    PLabel _ _ p -> checkClosed p
    PFun _ -> yes
    PPat _ -> yes
    PScape _ -> yes
    PConj _ p1 p2 -> checkClosed p1 >> checkClosed p2
    PSubst _ x (AstList _ ps) -> checkClosed x >> checkClosed ps
    PRec _ _ p -> checkClosed p
    PVar _ _ -> yes
    where yes = return ()
