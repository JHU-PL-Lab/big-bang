{-# LANGUAGE GADTs #-}

{-|
  Represents a monad transformer which describes multiple type flows.  This is
  a semantic wrapper over @ListT@.
-}
module Language.TinyBang.TypeSystem.Monad.Trans.Flow
( FlowT
, runFlowT
, flow
) where

import Control.Applicative (Applicative(..),(<$>),(<*>))
import Control.Monad (MonadPlus(..),mzero,mplus)
import Control.Monad.Trans.List (ListT(..), runListT)
import Data.Foldable

-- |A datatype for constraint flow computations.
data FlowT m a where
  FlowT :: ListT m a -> FlowT m a

-- Typeclass instances for FlowT
instance (Monad m) => Monad (FlowT m) where
  FlowT a >>= f = FlowT $ a >>= unFlowT . f
    where unFlowT (FlowT y) = y
  return = FlowT . return
  fail msg = FlowT $ fail msg
instance (Monad m) => MonadPlus (FlowT m) where
  mzero = FlowT mzero
  mplus (FlowT a) (FlowT b) = FlowT $ mplus a b
instance (Functor m) => Functor (FlowT m) where
  fmap f (FlowT x) = FlowT $ fmap f x
instance (Applicative m) => Applicative (FlowT m) where
  pure = FlowT . pure
  FlowT f <*> FlowT x = FlowT $ f <*> x

-- |Expands the underlying flow computation into its results.
runFlowT :: FlowT m a -> m [a]
runFlowT (FlowT listT) = runListT listT

-- |Transforms a foldable data structure into a flow.
flow :: (Foldable f, Functor m, Monad m) => m (f a) -> FlowT m a
flow xM = FlowT $ ListT $ toList <$> xM
