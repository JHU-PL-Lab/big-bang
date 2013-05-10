{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, FlexibleInstances #-}
module Language.TinyBang.TypeSystem.Monad.Trans.CReader
( CReaderT
, MonadCReader(..)
, CReader
, runCReader
, runCReaderT
)
where

import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadTrans)
import Control.Applicative (Applicative, Alternative)

type CReader db a = CReaderT db Identity a

runCReader :: CReader db a -> db -> a
runCReader m = runIdentity . runCReaderT m

runCReaderT :: CReaderT db m a -> db -> m a
runCReaderT (CReaderT m) = runReaderT m

newtype CReaderT db m a = CReaderT (ReaderT db m a)
  deriving (Monad, Functor, MonadTrans, Applicative, Alternative)

class MonadCReader db m | m -> db where
  askDB :: m db
  localDB :: (db -> db) -> m a -> m a

instance (Monad m) => MonadCReader db (CReaderT db m) where
  askDB = CReaderT ask
  localDB f (CReaderT m) = CReaderT $ local f m
