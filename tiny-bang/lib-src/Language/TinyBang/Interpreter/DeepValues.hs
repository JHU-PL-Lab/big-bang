{-# LANGUAGE ExistentialQuantification, GADTs, StandaloneDeriving #-}

module Language.TinyBang.Interpreter.DeepValues
( DeepOnion(..)
, DeepPrimitiveType(..)
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
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mappend, mempty)

import Language.TinyBang.Ast
import Language.TinyBang.Display hiding (empty)

-- |This data structure represents deeply-structured TinyBang values.  It can
--  be used after evaluation to represent a result in a more human-friendly
--  form.  This structure is lossy; it does not track the cells under labels,
--  instead merely representing their contents.
data DeepOnion = DeepOnion
      { deepOnionPrimitives :: DeepPrimitiveMap
      , deepOnionLabels :: Map LabelName DeepOnion
      , deepOnionScapes :: [(Pattern,Expr)]
      }
  deriving (Eq, Ord, Show)
  
data DeepPrimitiveType a where
  DeepPrimInt :: DeepPrimitiveType Integer 
  DeepPrimChar :: DeepPrimitiveType Char
  
deriving instance Eq (DeepPrimitiveType a)
deriving instance Ord (DeepPrimitiveType a)
deriving instance Show (DeepPrimitiveType a)

-- |Deep value conversion failures.
data ConversionFailure
  = UnboundFlowVariable FlowVar
  | UnboundCellVariable CellVar
  deriving (Eq, Ord, Show)
  
instance Display ConversionFailure where
  makeDoc = text . show
  
-- |A type for the deep onion conversion monad.
type DeepOnionConversionM a = ReaderT (Map FlowVar Value, Map CellVar FlowVar)
                                (Either ConversionFailure) a
                                
askFlowVarMap :: DeepOnionConversionM (Map FlowVar Value)
askFlowVarMap = fst <$> ask

askCellVarMap :: DeepOnionConversionM (Map CellVar FlowVar)
askCellVarMap = snd <$> ask

-- |A routine which will convert the result of evaluation into a deep value.
--  Note that this routine also flattens the value.  As a result, cyclic data
--  constructions such as infinite lists will produce infinite deep values.
--  If an error occurs during conversion, a @ConversionFailure@ is raised.
deepOnion :: Map FlowVar Value -> Map CellVar FlowVar -> FlowVar
          -> Either ConversionFailure DeepOnion
deepOnion fvm cvm x = runReaderT (deepOnionM x) (fvm,cvm)

deepOnionM :: FlowVar -> DeepOnionConversionM DeepOnion
deepOnionM x = do
  mval <- Map.lookup x <$> askFlowVarMap
  val <- lift $ note (UnboundFlowVariable x) mval
  case val of
    VInt _ n -> return DeepOnion
                  { deepOnionPrimitives = insert DeepPrimInt n empty
                  , deepOnionLabels = Map.empty
                  , deepOnionScapes = [] }
    VChar _ c -> return DeepOnion
                  { deepOnionPrimitives = insert DeepPrimChar c empty
                  , deepOnionLabels = Map.empty
                  , deepOnionScapes = [] }
    VEmptyOnion _ -> return mempty
    VLabel _ n y -> do
      mx' <- Map.lookup y <$> askCellVarMap
      x' <- lift $ note (UnboundCellVariable y) mx'
      ls <- Map.singleton n <$> deepOnionM x'
      return DeepOnion
                  { deepOnionPrimitives = empty
                  , deepOnionLabels = ls
                  , deepOnionScapes = [] }
    VOnion _ x' x'' -> mappend <$> deepOnionM x' <*> deepOnionM x''
    VOnionFilter _ x' op proj -> do
      DeepOnion prims labels scapes <- deepOnionM x'
      let (pOp,lOp,sOp) = case op of
            OpOnionSub _ -> (subPrims, subLabels, subScapes)
            OpOnionProj _ -> (projPrims, projLabels, projScapes)
      return $ DeepOnion (pOp proj prims) (lOp proj labels) (sOp proj scapes)
    VScape _ pattern expr -> return DeepOnion
                  { deepOnionPrimitives = empty
                  , deepOnionLabels = Map.empty
                  , deepOnionScapes = [(pattern,expr)] }
  where
    subPrims proj dpm@(DeepPrimitiveMap m) =
      case proj of
        SomeProjector (ProjPrim _ t) ->
          DeepPrimitiveMap $ flip Map.delete m $
            case t of
              PrimInt _ -> DeepPrimSInt
              PrimChar _ -> DeepPrimSChar
        _ -> dpm
    subLabels proj m =
      case proj of
        SomeProjector (ProjLabel _ n) -> Map.delete n m
        _ -> m
    subScapes proj ss =
      case proj of
        SomeProjector (ProjFun _) -> []
        _ -> ss
    projPrims proj m =
      case proj of
        SomeProjector (ProjPrim _ t) ->
          case t of
            PrimInt _ -> singletonOver DeepPrimInt
            PrimChar _ -> singletonOver DeepPrimChar
        _ -> empty
      where
        singletonOver :: DeepPrimitiveType a -> DeepPrimitiveMap
        singletonOver dp =
          case lookup dp m of
            Just v -> insert dp v empty
            Nothing -> empty
    projLabels proj m =
      case proj of
        SomeProjector (ProjLabel _ n) -> fromMaybe Map.empty $
                              Map.singleton n <$> Map.lookup n m
        _ -> Map.empty
    projScapes proj ss =
      case proj of
        SomeProjector (ProjFun _) -> ss
        _ -> []
        
instance Monoid DeepOnion where
  mempty = DeepOnion { deepOnionPrimitives = empty
                     , deepOnionLabels = Map.empty
                     , deepOnionScapes = [] }
  mappend (DeepOnion p1 l1 s1) (DeepOnion p2 l2 s2) = DeepOnion p' l' s'
    where
      p' = let DeepPrimitiveMap m1 = p1 in
           let DeepPrimitiveMap m2 = p2 in
           DeepPrimitiveMap $
             m2 `Map.union` m1
      l' = l2 `Map.union` l1
      s' = s1 ++ s2
      
componentCount :: DeepOnion -> Int
componentCount (DeepOnion (DeepPrimitiveMap prims) labels scapes) =
  Map.size prims + Map.size labels + length scapes 
      
instance Display DeepOnion where
  makeDoc = makeDeepOnionDoc
  
makeDeepOnionDoc :: DeepOnion -> Doc
makeDeepOnionDoc onion@(DeepOnion (DeepPrimitiveMap m) labels scapes) =
  if componentCount onion == 0
    then string "()"
    else sepDoc (char '&') docs
  where
    docs = map makePrimDoc (Map.toList m) ++
           map makeLabelDoc (Map.toList labels) ++
           map makeScapeDoc scapes
    makePrimDoc (_,v) = case v of
      DeepPrimVInt n -> makeDoc n
      DeepPrimVChar c -> char '\'' <> char c <> char '\''
    makeLabelDoc (n,onion') =
      let doc = makeDoc n <+> makeDeepOnionDoc onion' in
      if componentCount onion' <= 1
        then doc
        else char '(' <> doc <> char ')'
    makeScapeDoc (pat,expr) =
      char '(' <> makeDoc pat <+> string "->" <+> makeDoc expr <> char ')' 

data DeepPrimitiveSimpleType
  = DeepPrimSInt
  | DeepPrimSChar
  deriving (Eq, Ord, Show)
      
newtype DeepPrimitiveMap = DeepPrimitiveMap
  (Map DeepPrimitiveSimpleType DeepPrimitiveValue)
  deriving (Eq, Ord, Show)
  
data DeepPrimitiveValue
  = DeepPrimVInt Integer
  | DeepPrimVChar Char
  deriving (Eq, Ord, Show)

lookup :: DeepPrimitiveType a -> DeepPrimitiveMap -> Maybe a
lookup t (DeepPrimitiveMap m) =
  let invalid :: (Show a) => a -> b
      invalid val =
        error $ "Invalid map entry for type " ++ show t ++ " discovered: "
                ++ show val in
  case t of
    DeepPrimInt -> do
      val <- Map.lookup DeepPrimSInt m
      case val of
        DeepPrimVInt n -> Just n
        _ -> invalid val
    DeepPrimChar -> do
      val <- Map.lookup DeepPrimSChar m
      case val of
        DeepPrimVChar c -> Just c
        _ -> invalid val

insert :: DeepPrimitiveType a -> a -> DeepPrimitiveMap -> DeepPrimitiveMap
insert t val (DeepPrimitiveMap m) = DeepPrimitiveMap $ Map.insert k v m
  where
    (k,v) = case (t,val) of
              (DeepPrimInt, n) -> (DeepPrimSInt, DeepPrimVInt n)
              (DeepPrimChar, c) -> (DeepPrimSChar, DeepPrimVChar c)

delete :: DeepPrimitiveType a -> DeepPrimitiveMap -> DeepPrimitiveMap
delete t (DeepPrimitiveMap m) = DeepPrimitiveMap $ Map.delete k m
  where
    k = case t of
          DeepPrimInt -> DeepPrimSInt
          DeepPrimChar -> DeepPrimSChar

empty :: DeepPrimitiveMap
empty = DeepPrimitiveMap Map.empty
