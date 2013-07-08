{-# LANGUAGE ExistentialQuantification, GADTs, StandaloneDeriving #-}

module Language.PatBang.Interpreter.DeepValues
( DeepOnion(..)
, DeepBasicType(..)
, ConversionFailure(..)
, deepOnion
, lookup
, insert
, delete
, empty
) where

import Prelude hiding (lookup)

import Control.Applicative ((<$>),(<*>))
import Control.Error.Util
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Monoid, mappend, mempty)

import Language.PatBang.Ast
import Language.PatBang.Display hiding (empty)

-- |This data structure represents deeply-structured PatBang values.  It can
--  be used after evaluation to represent a result in a more human-friendly
--  form.  This structure is lossy; it does not track the cells under labels,
--  instead merely representing their contents.
data DeepOnion = DeepOnion
      { deepOnionBasics :: DeepBasicMap
      , deepOnionLabels :: Map LabelName DeepOnion
      , deepOnionScapes :: [(Value,Value)] -- pattern and function
      }
  deriving (Eq, Ord, Show)
  
data DeepBasicType a where
  DeepBasicInt :: DeepBasicType Integer
  DeepBasicFun :: DeepBasicType Value
  DeepBasicPat :: DeepBasicType Value
  
deriving instance Eq (DeepBasicType a)
deriving instance Ord (DeepBasicType a)
deriving instance Show (DeepBasicType a)

-- |Deep value conversion failures.
data ConversionFailure
  = UnboundFlowVariable FlowVar
  deriving (Eq, Ord, Show)
  
instance Display ConversionFailure where
  makeDoc = text . show
  
-- |A type for the deep onion conversion monad.
type DeepOnionConversionM a = ReaderT (Map FlowVar Value)
                                (Either ConversionFailure) a
                                
askFlowVarMap :: DeepOnionConversionM (Map FlowVar Value)
askFlowVarMap = ask

-- |A routine which will convert the result of evaluation into a deep value.
--  Note that this routine also flattens the value.  As a result, cyclic data
--  constructions such as infinite lists will produce infinite deep values.
--  If an error occurs during conversion, a @ConversionFailure@ is raised.
deepOnion :: Map FlowVar Value -> FlowVar
          -> Either ConversionFailure DeepOnion
deepOnion fvm x = runReaderT (deepOnionM x) fvm

deepOnionM :: FlowVar -> DeepOnionConversionM DeepOnion
deepOnionM x = do
  mval <- Map.lookup x <$> askFlowVarMap
  val <- lift $ note (UnboundFlowVariable x) mval
  case val of
    VInt _ n -> return DeepOnion
                  { deepOnionBasics = insert DeepBasicInt n empty
                  , deepOnionLabels = Map.empty
                  , deepOnionScapes = [] }
    VEmptyOnion _ -> return mempty
    VLabel _ n x' -> do
      ls <- Map.singleton n <$> deepOnionM x'
      return DeepOnion
                  { deepOnionBasics = empty
                  , deepOnionLabels = ls
                  , deepOnionScapes = [] }
    VOnion _ x' x'' -> mappend <$> deepOnionM x' <*> deepOnionM x''
    VFunction _ _ _ ->
      return DeepOnion
                  { deepOnionBasics = insert DeepBasicFun val empty
                  , deepOnionLabels = Map.empty
                  , deepOnionScapes = [] }
    VPattern _ _ _ ->
      return DeepOnion
                  { deepOnionBasics = insert DeepBasicPat val empty
                  , deepOnionLabels = Map.empty
                  , deepOnionScapes = [] }
    VScape _ x' x'' -> do
      mx'val <- Map.lookup x' <$> askFlowVarMap
      x'val <- lift $ note (UnboundFlowVariable x') mx'val
      mx''val <- Map.lookup x'' <$> askFlowVarMap
      x''val <- lift $ note (UnboundFlowVariable x'') mx''val
      return DeepOnion
                  { deepOnionBasics = empty
                  , deepOnionLabels = Map.empty
                  , deepOnionScapes = [(x'val,x''val)] }

instance Monoid DeepOnion where
  mempty = DeepOnion { deepOnionBasics = empty
                     , deepOnionLabels = Map.empty
                     , deepOnionScapes = [] }
  mappend (DeepOnion p1 l1 s1) (DeepOnion p2 l2 s2) = DeepOnion p' l' s'
    where
      p' = let DeepBasicMap m1 = p1 in
           let DeepBasicMap m2 = p2 in
           DeepBasicMap $
             m2 `Map.union` m1
      l' = l2 `Map.union` l1
      s' = s1 ++ s2
      
componentCount :: DeepOnion -> Int
componentCount (DeepOnion (DeepBasicMap prims) labels scapes) =
  Map.size prims + Map.size labels + length scapes 
      
instance Display DeepOnion where
  makeDoc = makeDeepOnionDoc
  
makeDeepOnionDoc :: DeepOnion -> Doc
makeDeepOnionDoc onion@(DeepOnion (DeepBasicMap m) labels scapes) =
  if componentCount onion == 0
    then string "()"
    else sepDoc (char '&') docs
  where
    docs = map makeBasicDoc (Map.toList m) ++
           map makeLabelDoc (Map.toList labels) ++
           map makeScapeDoc scapes
    makeBasicDoc (_,v) = case v of
      DeepBasicVInt n -> makeDoc n
      DeepBasicVFun v' -> makeDoc v'
      DeepBasicVPat v' -> makeDoc v'
    makeLabelDoc (n,onion') =
      let doc = makeDoc n <+> makeDeepOnionDoc onion' in
      if componentCount onion' <= 1
        then doc
        else char '(' <> doc <> char ')'
    makeScapeDoc (pat,expr) =
      char '(' <> makeDoc pat <+> string "->" <+> makeDoc expr <> char ')' 

data DeepBasicSimpleType
  = DeepBasicSInt
  | DeepBasicSFun
  | DeepBasicSPat
  deriving (Eq, Ord, Show)
      
newtype DeepBasicMap = DeepBasicMap
  (Map DeepBasicSimpleType DeepBasicValue)
  deriving (Eq, Ord, Show)
  
data DeepBasicValue
  = DeepBasicVInt Integer
  | DeepBasicVFun Value
  | DeepBasicVPat Value
  deriving (Eq, Ord, Show)

lookup :: DeepBasicType a -> DeepBasicMap -> Maybe a
lookup t (DeepBasicMap m) =
  let invalid :: (Show a) => a -> b
      invalid val =
        error $ "Invalid map entry for type " ++ show t ++ " discovered: "
                ++ show val in
  case t of
    DeepBasicInt -> do
      val <- Map.lookup DeepBasicSInt m
      case val of
        DeepBasicVInt n -> Just n
        _ -> invalid val
    DeepBasicFun -> do
      val <- Map.lookup DeepBasicSFun m
      case val of
        DeepBasicVFun v -> Just v
        _ -> invalid val
    DeepBasicPat -> do
      val <- Map.lookup DeepBasicSPat m
      case val of
        DeepBasicVPat v -> Just v
        _ -> invalid val

insert :: DeepBasicType a -> a -> DeepBasicMap -> DeepBasicMap
insert t val (DeepBasicMap m) = DeepBasicMap $ Map.insert k v m
  where
    (k,v) = case (t,val) of
              (DeepBasicInt, n) -> (DeepBasicSInt, DeepBasicVInt n)
              (DeepBasicFun, v') -> (DeepBasicSFun, DeepBasicVFun v')
              (DeepBasicPat, v') -> (DeepBasicSPat, DeepBasicVPat v')

delete :: DeepBasicType a -> DeepBasicMap -> DeepBasicMap
delete t (DeepBasicMap m) = DeepBasicMap $ Map.delete k m
  where
    k = case t of
          DeepBasicInt -> DeepBasicSInt
          DeepBasicFun -> DeepBasicSFun
          DeepBasicPat -> DeepBasicSPat

empty :: DeepBasicMap
empty = DeepBasicMap Map.empty
