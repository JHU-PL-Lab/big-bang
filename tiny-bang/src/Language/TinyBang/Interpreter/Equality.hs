module Language.TinyBang.Interpreter.Equality
( flowVarValueEq
) where

import Control.Applicative ((<$>),(<*>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Monoid, mappend, mempty)

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis

-- |Calculates equality for the contents of two flow variables.
flowVarValueEq :: FlowVar -> FlowVar -> EvalM Bool
flowVarValueEq x x' = do
  FlattenedOnion p1 l1 s1 <- flowVarFlatten x
  FlattenedOnion p2 l2 s2 <- flowVarFlatten x'
  lequal <- mlequal l1 l2
  return $ (p1 == p2) && lequal && (s1 == s2)
  where
    mlequal :: Map LabelName CellVar -> Map LabelName CellVar -> EvalM Bool
    mlequal m1 m2 = do
      if Map.keysSet m1 /= Map.keysSet m2
        then return False
        else
          let cellVarPairs = Map.elems $ Map.mapWithKey f m1 in
          and <$> mapM cellVarValueEq cellVarPairs
      where
        f :: LabelName -> CellVar -> (CellVar, CellVar)
        f n c = (c, (Map.!) m2 n)
        cellVarValueEq :: (CellVar, CellVar) -> EvalM Bool
        cellVarValueEq (y,y') = do
          xn <- cellLookup y
          x'n <- cellLookup y'
          flowVarValueEq xn x'n

-- |Flattens a given flow variable into a type-indexed record.
flowVarFlatten :: FlowVar -> EvalM FlattenedOnion
flowVarFlatten x = do
  v <- flowLookup x
  case v of
    VInt _ _ -> return $ FlattenedOnion (Map.singleton primInt v) Map.empty []
    VChar _ _ -> return $ FlattenedOnion (Map.singleton primChar v) Map.empty []
    VEmptyOnion _ -> return mempty
    VLabel _ n y -> return $ FlattenedOnion Map.empty (Map.singleton n y) []
    VOnion _ x' x'' -> mappend <$> flowVarFlatten x' <*> flowVarFlatten x''
    VOnionFilter _ x' op proj -> do
      let (pkeep,lkeep,skeep) = case op of
            OpOnionProj _ -> ( primMatchProjector proj
                             , labelMatchProjector proj
                             , scapeMatchProjector proj )
            OpOnionSub _ -> ( not . primMatchProjector proj
                            , not . labelMatchProjector proj
                            , not $ scapeMatchProjector proj )
      FlattenedOnion p l s <- flowVarFlatten x'
      return $ FlattenedOnion
        (Map.filterWithKey (toss2nd pkeep) p)
        (Map.filterWithKey (toss2nd lkeep) l)
        (if skeep then s else [])
    VScape _ _ _ -> return $ FlattenedOnion Map.empty Map.empty [v]
  where
    primMatchProjector :: Projector -> PrimitiveType -> Bool
    primMatchProjector proj p = case proj of
      ProjPrim _ p' -> p == p'
      _ -> False
    labelMatchProjector :: Projector -> LabelName -> Bool
    labelMatchProjector proj n = case proj of
      ProjLabel _ n' -> n == n'
      _ -> False
    scapeMatchProjector :: Projector -> Bool
    scapeMatchProjector proj = case proj of
      ProjFun _ -> True
      _ -> False
    toss2nd :: (a -> b) -> a -> c -> b
    toss2nd f a _ = f a

-- |A data type for flattened onions.
data FlattenedOnion
  = FlattenedOnion
      (Map PrimitiveType Value) -- ^ The primitive types in the onion
      (Map LabelName CellVar)   -- ^ The labels in the onion 
      ([Value])                 -- ^ The scape values in the onion

instance Monoid FlattenedOnion where
  mempty = FlattenedOnion Map.empty Map.empty []
  mappend (FlattenedOnion p1 l1 s1) (FlattenedOnion p2 l2 s2) =
    FlattenedOnion (Map.union p2 p1) (Map.union l2 l1) (s1 ++ s2)
