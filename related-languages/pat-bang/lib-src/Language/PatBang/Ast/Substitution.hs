{-# LANGUAGE FlexibleInstances #-}

{-|
  Contains routines for performing substitution on an AST.
-}

module Language.PatBang.Ast.Substitution
( Substitution(..)
, Substitutions(..)
, collect
, substitute
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Language.PatBang.Ast.Data
import Language.PatBang.Display

-- |A data type for substitutions.  (The first tuple component is the new cell
--  variable; the second is the variable to be replaced.)
data Substitution
  = FlowSubstitution
      FlowVar -- ^ The flow variable to use
      FlowVar -- ^ The flow variable to replace
  deriving (Eq, Ord, Show)

-- |A data type representing a collection of substitutions.
data Substitutions
  = Substitutions { flowSubsts :: Map FlowVar FlowVar }

-- |Converts a list of substitutions into a collection of substitutions.
collect :: [Substitution] -> Substitutions
collect = foldr addSubstitution Substitutions
            { flowSubsts = Map.empty }
  where
    addSubstitution :: Substitution -> Substitutions -> Substitutions
    addSubstitution s ss =
      case s of
        FlowSubstitution x x' ->
          ss { flowSubsts = Map.insert x' x $ flowSubsts ss }

-- |Performs a series of substitutions on a given expression.
substitute :: Substitutions -> Expr -> Expr
substitute = subst

class Substitutable a where
  subst :: Substitutions -> a -> a

instance (Substitutable a) => Substitutable [a] where
  subst ss = map (subst ss)

instance Substitutable Expr where
  subst ss (Expr orig cls) = Expr orig $ subst ss cls

instance Substitutable Clause where
  subst ss arg = case arg of
    RedexDef orig x r -> RedexDef orig (subst ss x) (subst ss r)
    ValueDef orig x v -> ValueDef orig (subst ss x) (subst ss v)

instance Substitutable Redex where
  subst ss arg = case arg of
    Define orig x -> Define orig $ subst ss x
    Appl orig x x' -> Appl orig (subst ss x) (subst ss x')
    BinOp orig x op x' -> BinOp orig (subst ss x) op (subst ss x')
    
instance (Substitutable a) => Substitutable (AstList a) where
  subst ss arg = case arg of
    AstList orig els -> AstList orig $ map (subst ss) els

instance Substitutable Value where
  subst ss arg = case arg of
    VInt _ _ -> arg
    VEmptyOnion _ -> arg
    VLabel orig n y -> VLabel orig n $ subst ss y
    VOnion orig x x' -> VOnion orig (subst ss x) (subst ss x')
    VFunction orig xs e -> VFunction orig (subst ss xs) (subst ss e)
    VPattern orig ys p -> VPattern orig (subst ss ys) (subst ss p)
    VScape orig x x' -> VScape orig (subst ss x) (subst ss x')
    
instance Substitutable PatternBody where
  subst ss arg = case arg of
    PPrim orig t -> PPrim orig t
    PLabel orig n p -> PLabel orig n $ subst ss p
    PFun orig -> PFun orig
    PPat orig -> PPat orig
    PScape orig -> PScape orig
    PConj orig p1 p2 -> PConj orig (subst ss p1) (subst ss p2)
    PDisj orig p1 p2 -> PDisj orig (subst ss p1) (subst ss p2)
    PSubst orig x ps -> PSubst orig (subst ss x) (subst ss ps)
    PRec orig y p -> PRec orig (subst ss y) (subst ss p)
    PPatternOf orig x -> PPatternOf orig $ subst ss x
    PVar orig y -> PVar orig (subst ss y)
    PNone orig -> PNone orig

instance Substitutable FlowVar where
  subst ss x = fromMaybe x $ Map.lookup x $ flowSubsts ss

instance Substitutable PatVar where
  subst _ y = y

instance Display Substitution where
  makeDoc s = case s of
    FlowSubstitution x x' -> makeDoc x <> char '/' <> makeDoc x'

instance Display Substitutions where
  makeDoc ss = makeDoc (flowSubsts ss)
