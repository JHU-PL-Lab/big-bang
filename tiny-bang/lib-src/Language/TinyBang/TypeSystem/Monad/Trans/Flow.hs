{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
  Represents a monad transformer which describes multiple type flows.  This is
  a semantic wrapper over @ListT@.
-}
module Language.TinyBang.TypeSystem.Monad.Trans.Flow
( FlowT
, runFlowT
, flow
) where

import Control.Applicative (Applicative(..),(<$>))
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.List (ListT(..), runListT)
import Data.Foldable

-- |A datatype for constraint flow computations.
newtype FlowT m a = FlowT (ListT m a)
  deriving (Monad, Functor, Applicative, MonadTrans, MonadPlus)
  
-- |Expands the underlying flow computation into its results.
runFlowT :: FlowT m a -> m [a]
runFlowT (FlowT listT) = runListT listT

-- |Transforms a foldable data structure into a flow.
flow :: (Foldable f, Functor m, Monad m) => m (f a) -> FlowT m a
flow xM = FlowT $ ListT $ toList <$> xM
