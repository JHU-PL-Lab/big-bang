{-# LANGUAGE FlexibleInstances #-}

{-|
  Contains routines for performing substitution on an AST.
-}

module Language.TinyBang.Ast.Substitution
( Substitution(..)
, Substitutions(..)
, collect
, substitute
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Language.TinyBang.Ast.Data
import Language.TinyBang.Display

-- |A data type for substitutions.  (The first tuple component is the new cell
--  variable; the second is the variable to be replaced.)
data Substitution
  = CellSubstitution
      CellVar -- ^ The cell variable to use
      CellVar -- ^ The cell variable to replace
  | FlowSubstitution
      FlowVar -- ^ The flow variable to use
      FlowVar -- ^ The flow variable to replace
  deriving (Eq, Ord, Show)

-- |A data type representing a collection of substitutions.
data Substitutions
  = Substitutions { flowSubsts :: Map FlowVar FlowVar
                  , cellSubsts :: Map CellVar CellVar }

-- |Converts a list of substitutions into a collection of substitutions.
collect :: [Substitution] -> Substitutions
collect = foldr addSubstitution Substitutions
            { cellSubsts = Map.empty 
            , flowSubsts = Map.empty }
  where
    addSubstitution :: Substitution -> Substitutions -> Substitutions
    addSubstitution s ss =
      case s of
        CellSubstitution y y' ->
          ss { cellSubsts = Map.insert y' y $ cellSubsts ss }
        FlowSubstitution x x' ->
          ss { flowSubsts = Map.insert x' x $ flowSubsts ss }

-- |Performs a series of substitutions on a given expression.
substitute :: Substitutions -> Expr -> Expr
substitute = subst

class Substitutable a where
  subst :: Substitutions -> a -> a

instance Substitutable Expr where
  subst ss (Expr orig cls) = Expr orig $ map (subst ss) cls

instance Substitutable Clause where
  subst ss arg = case arg of
    RedexDef orig x r -> RedexDef orig (subst ss x) (subst ss r)
    CellSet orig y x -> CellSet orig (subst ss y) (subst ss x)
    CellGet orig x y -> CellGet orig (subst ss x) (subst ss y)
    Throws orig x x' -> Throws orig (subst ss x) (subst ss x')
    Evaluated cl -> Evaluated $ subst ss cl

instance Substitutable EvaluatedClause where
  subst ss arg = case arg of
    ValueDef orig x v -> ValueDef orig (subst ss x) (subst ss v)
    CellDef orig q y x -> CellDef orig q (subst ss y) (subst ss x)
    Flow orig x k x' -> Flow orig (subst ss x) k (subst ss x')

instance Substitutable Redex where
  subst ss arg = case arg of
    Define orig x -> Define orig $ subst ss x
    Appl orig x x' -> Appl orig (subst ss x) (subst ss x')
    BinOp orig x op x' -> BinOp orig (subst ss x) op (subst ss x')

instance Substitutable Value where
  subst ss arg = case arg of
    VInt _ _ -> arg
    VChar _ _ -> arg
    VEmptyOnion _ -> arg
    VLabel orig n y -> VLabel orig n $ subst ss y
    VOnion orig x x' -> VOnion orig (subst ss x) (subst ss x')
    VOnionFilter orig x op proj -> VOnionFilter orig (subst ss x) op proj
    VScape orig pat e -> VScape orig (subst ss pat) (subst ss e)

instance Substitutable Pattern where
  subst ss arg = case arg of
    ValuePattern orig y ipat -> ValuePattern orig (subst ss y) (subst ss ipat)
    ExnPattern orig y ipat -> ExnPattern orig (subst ss y) (subst ss ipat)

instance Substitutable InnerPattern where
  subst ss arg = case arg of
    PrimitivePattern _ _ -> arg
    LabelPattern orig n y ipat ->
      LabelPattern orig n (subst ss y) (subst ss ipat)
    ConjunctionPattern orig ipat ipat' ->
      ConjunctionPattern orig (subst ss ipat) (subst ss ipat')
    ScapePattern _ -> arg
    EmptyOnionPattern _ -> arg

instance Substitutable FlowVar where
  subst ss x = fromMaybe x $ Map.lookup x $ flowSubsts ss

instance Substitutable CellVar where
  subst ss y = fromMaybe y $ Map.lookup y $ cellSubsts ss

instance Display Substitution where
  makeDoc s = case s of
    CellSubstitution y y' -> makeDoc y <> char '/' <> makeDoc y'
    FlowSubstitution x x' -> makeDoc x <> char '/' <> makeDoc x'

instance Display Substitutions where
  makeDoc ss = makeDoc (flowSubsts ss, cellSubsts ss)
