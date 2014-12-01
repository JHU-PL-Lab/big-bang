{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TupleSections #-}

{-|
  Represents a monad transformer which describes multiple type flows.  This is
  a semantic wrapper over @ListT@; it is used to signify intentional
  non-deterministic computation.
-}
module Control.Monad.Trans.NonDet
( NonDetT
, runNonDetT
, mapNonDetT
, MonadNonDet(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Either
import Control.Monad.Trans.List
import Data.Foldable

-- |A datatype for constraint flow computations.
newtype NonDetT m a
  = NonDetT (ListT m a)
  deriving (Monad, Functor, Applicative, MonadTrans, MonadPlus)
  
-- |Expands the underlying flow computation into its results.
runNonDetT :: NonDetT m a -> m [a]
runNonDetT (NonDetT listT) = runListT listT

-- |Maps an operation through a @NonDetT@.
mapNonDetT :: (m [a] -> n [b]) -> NonDetT m a -> NonDetT n b
mapNonDetT f (NonDetT x) = NonDetT $ mapListT f x

instance MonadReader r m => MonadReader r (NonDetT m) where
  ask = lift ask
  local f (NonDetT x) = NonDetT $ local f x

-- NOTE: MonadWriter cannot be defined over NonDetT because listen and pass
--       cannot be defined on it sensibly.
  
class (Monad m) => MonadNonDet m where
  choose :: (Foldable f) => f a -> m a

instance (Monad m) => MonadNonDet (NonDetT m) where
  choose x = NonDetT $ ListT $ toList `liftM` return x

instance (MonadNonDet m) => MonadNonDet (ReaderT r m) where
  choose = lift . choose

instance (MonadNonDet m) => MonadNonDet (StateT s m) where
  choose = lift . choose

instance (Monoid w, MonadNonDet m) => MonadNonDet (WriterT w m) where
  choose = lift . choose

instance (MonadNonDet m) => MonadNonDet (EitherT e m) where
  choose = lift . choose
