{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

{-|
  Represents a monad transformer which describes multiple type flows.  This is
  a semantic wrapper over @ListT@.
-}
module Language.TinyBang.TypeSystem.Monad.Trans.Flow
( FlowT
, runFlowT
, mapFlowT
, flow
) where

import Control.Applicative (Applicative(..),(<$>))
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.List (ListT(..), runListT, mapListT)
import Data.Foldable

import Language.TinyBang.TypeSystem.Monad.Trans.CReader

-- |A datatype for constraint flow computations.
newtype FlowT m a = FlowT (ListT m a)
  deriving (Monad, Functor, Applicative, MonadTrans, MonadPlus)
  
-- |Expands the underlying flow computation into its results.
runFlowT :: FlowT m a -> m [a]
runFlowT (FlowT listT) = runListT listT

-- |Maps an operation through a @FlowT@.
mapFlowT :: (m [a] -> n [b]) -> FlowT m a -> FlowT n b
mapFlowT f (FlowT x) = FlowT $ mapListT f x

-- |Transforms a foldable data structure into a flow.
flow :: (Foldable f, Functor m, Monad m) => m (f a) -> FlowT m a
flow xM = FlowT $ ListT $ toList <$> xM

instance MonadCReader r m => MonadCReader r (FlowT m) where
    askDb   = lift askDb
    localDb f (FlowT x) = FlowT $ localDb f x
