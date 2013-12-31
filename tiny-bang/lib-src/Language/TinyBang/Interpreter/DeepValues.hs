{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Language.TinyBang.Interpreter.DeepValues
( DeepOnion(..)
, ConversionFailure(..)
, deepOnion
) where

import Prelude hiding (lookup)

import Control.Applicative ((<$>),(<*>))
import Control.Error.Util
import Control.Monad.Reader
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Monoid, mappend, mempty)

import Language.TinyBang.Ast
import Language.TinyBang.Display hiding (empty)

-- |This data structure represents deeply-structured TinyBang values.  It can
--  be used after evaluation to represent a result in a more human-friendly
--  form.  This structure is lossy; it does not track the cells under labels,
--  instead merely representing their contents.
data DeepOnion = DeepOnion
      { deepPrimitives :: Map PrimitiveType PrimitiveValue
      , deepLabels :: Map LabelName DeepOnion
      , deepScapes :: [(Pattern,Expr)]
      }
  deriving (Eq, Ord, Show)
  
-- |Deep value conversion failures.
data ConversionFailure
  = UnboundVariable Var
  deriving (Eq, Ord, Show)
  
instance Display ConversionFailure where
  makeDoc = text . show
  
-- |A type for the deep onion conversion monad.
type DeepOnionConversionM a
  = ReaderT (Map Var Value) (Either ConversionFailure) a
                                
askVarMap :: DeepOnionConversionM (Map Var Value)
askVarMap = ask

-- |A routine which will convert the result of evaluation into a deep value.
--  Note that this routine also flattens the value.  As a result, cyclic data
--  constructions such as infinite lists will produce infinite deep values.
--  If an error occurs during conversion, a @ConversionFailure@ is raised.
deepOnion :: Map Var Value -> Var -> Either ConversionFailure DeepOnion
deepOnion fvm x = runReaderT (deepOnionM x) fvm

deepOnionM :: Var -> DeepOnionConversionM DeepOnion
deepOnionM x = do
  mval <- Map.lookup x <$> askVarMap
  val <- lift $ note (UnboundVariable x) mval
  case val of
    VPrimitive _ prim ->
      return DeepOnion
        { deepPrimitives = Map.singleton (typeOfPrimitiveValue prim) prim
        , deepLabels = Map.empty
        , deepScapes = [] }
    VEmptyOnion _ -> return mempty
    VLabel _ n x' -> do
      ls <- Map.singleton n <$> deepOnionM x'
      return DeepOnion
        { deepPrimitives = Map.empty
        , deepLabels = ls
        , deepScapes = [] }
    VOnion _ x' x'' -> mappend <$> deepOnionM x' <*> deepOnionM x''
    VScape _ pattern expr -> return DeepOnion
                  { deepPrimitives = Map.empty
                  , deepLabels = Map.empty
                  , deepScapes = [(pattern,expr)] }
        
instance Monoid DeepOnion where
  mempty = DeepOnion { deepPrimitives = Map.empty
                     , deepLabels = Map.empty
                     , deepScapes = [] }
  mappend (DeepOnion p1 l1 s1) (DeepOnion p2 l2 s2) = DeepOnion p' l' s'
    where
      p' = p1 `Map.union` p2
      l' = l1 `Map.union` l2
      s' = s1 ++ s2

instance Display DeepOnion where
  makeDoc = joinComponents . componentsOf
    where
      joinComponents comps =
        if null comps then text "()" else
          foldl1 (<+>) $ intersperse (text " & ") comps
      componentsOf x = primComponents ++ labelComponents ++ funComponents
        where
          primComponents = map (makeDoc . snd) $ Map.toList $ deepPrimitives x
          labelComponents =
            map (uncurry labelToDoc) $ Map.toList $ deepLabels x
            where
              labelToDoc label inner =
                let cs = componentsOf inner in
                (<+>) (makeDoc label) $
                  (case cs of
                    _:_:_ -> parens -- 2+ elements
                    _ -> id) $ joinComponents cs
          funComponents =
            map (\(a,b) -> parens $ makeDoc a <+> text " -> " <+> makeDoc b) $
              deepScapes x