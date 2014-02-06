{-|
  Common routines used by the generated code from
  @Data.IndexedSet.TemplateHaskell@.
-}
module Data.IndexedSet.Common
( MultiMap
, mapToMultiMap
, appendMultiMap
, concatMultiMap
, multiMapLookup
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type MultiMap k v = Map k (Set v)

mapToMultiMap :: (Ord k, Ord v) => Map k v -> MultiMap k v
mapToMultiMap = Map.map Set.singleton

appendMultiMap :: (Ord k, Ord v) => MultiMap k v -> MultiMap k v -> MultiMap k v
appendMultiMap = Map.unionWith Set.union

concatMultiMap :: (Ord k, Ord v) => [MultiMap k v] -> MultiMap k v
concatMultiMap = Map.unionsWith Set.union

multiMapLookup :: (Ord k, Ord v) => k -> MultiMap k v -> Set v
multiMapLookup k = fromMaybe Set.empty . Map.lookup k
