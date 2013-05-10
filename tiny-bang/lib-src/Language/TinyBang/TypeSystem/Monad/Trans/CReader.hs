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

import Language.TinyBang.TypeSystem.ConstraintDatabase

type CReader db a = CReaderT db Identity a

runCReader :: ConstraintDatabase db => CReader db a -> db -> a
runCReader m = runIdentity . runCReaderT m

runCReaderT :: ConstraintDatabase db => CReaderT db m a -> db -> m a
runCReaderT (CReaderT m) = runReaderT m

newtype CReaderT db m a = CReaderT (ReaderT db m a)
  deriving (Monad, Functor, MonadTrans, Applicative, Alternative)

class ConstraintDatabase db => MonadCReader db m | m -> db where
  askDb :: m db
  localDb :: (db -> db) -> m a -> m a

instance (Monad m, ConstraintDatabase db) => MonadCReader db (CReaderT db m) where
  askDb = CReaderT ask
  localDb f (CReaderT m) = CReaderT $ local f m
