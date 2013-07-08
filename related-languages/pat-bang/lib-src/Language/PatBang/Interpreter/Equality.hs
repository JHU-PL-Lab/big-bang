{-# LANGUAGE GADTs #-}

module Language.PatBang.Interpreter.Equality
( flowVarValueEq
) where

import Control.Applicative ((<$>),(<*>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Monoid, mappend, mempty)

import Language.PatBang.Ast
import Language.PatBang.Interpreter.Basis

-- |Calculates equality for the contents of two flow variables.
flowVarValueEq :: FlowVar -> FlowVar -> EvalM Bool
flowVarValueEq x x' = do
  FlattenedOnion p1 l1 s1 <- flowVarFlatten x
  FlattenedOnion p2 l2 s2 <- flowVarFlatten x'
  lequal <- mlequal l1 l2
  return $ (p1 == p2) && lequal && (s1 == s2)
  where
    mlequal :: Map LabelName FlowVar -> Map LabelName FlowVar -> EvalM Bool
    mlequal m1 m2 = do
      if Map.keysSet m1 /= Map.keysSet m2
        then return False
        else
          and <$> mapM (uncurry flowVarValueEq)
                    (Map.elems $ Map.intersectionWith (,) m1 m2)

-- |Flattens a given flow variable into a type-indexed record.
flowVarFlatten :: FlowVar -> EvalM FlattenedOnion
flowVarFlatten x = do
  v <- flowLookup x
  case v of
    VInt _ _ -> return $ FlattenedOnion
                  (Map.singleton (BasicPrimitiveType primInt) v) Map.empty []
    VEmptyOnion _ -> return mempty
    VLabel _ n y -> return $ FlattenedOnion Map.empty (Map.singleton n y) []
    VOnion _ x' x'' -> mappend <$> flowVarFlatten x' <*> flowVarFlatten x''
    VFunction _ _ _ -> return $ FlattenedOnion
                          (Map.singleton BasicFunctionType v) Map.empty []
    VPattern _ _ _ -> return $ FlattenedOnion
                          (Map.singleton BasicPatternType v) Map.empty []
    VScape _ x' x'' -> return $ FlattenedOnion Map.empty Map.empty [v]

-- |A data type for flattened onions.
data FlattenedOnion
  = FlattenedOnion
      (Map BasicType Value)     -- ^ The basic types in the onion
      (Map LabelName FlowVar)   -- ^ The labels in the onion 
      ([Value])                 -- ^ The scape values in the onion

data BasicType
  = BasicPrimitiveType PrimitiveType
  | BasicFunctionType
  | BasicPatternType
  deriving (Eq, Ord, Show)

instance Monoid FlattenedOnion where
  mempty = FlattenedOnion Map.empty Map.empty []
  mappend (FlattenedOnion p1 l1 s1) (FlattenedOnion p2 l2 s2) =
    FlattenedOnion (Map.union p2 p1) (Map.union l2 l1) (s1 ++ s2)
