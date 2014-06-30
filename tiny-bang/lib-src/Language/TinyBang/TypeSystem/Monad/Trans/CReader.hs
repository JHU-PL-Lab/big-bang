{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Language.TinyBang.TypeSystem.Monad.Trans.CReader
( CReaderT
, MonadCReader(..)
, CReader
, runCReader
, runCReaderT
)
where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Set (Set)

import Language.TinyBang.TypeSystem.ConstraintDatabase

type CReader db = CReaderT db Identity

runCReader :: ConstraintDatabase db => CReader db a -> db -> a
runCReader m = runIdentity . runCReaderT m

runCReaderT :: ConstraintDatabase db => CReaderT db m a -> db -> m a
runCReaderT (CReaderT m) = runReaderT m

newtype CReaderT db m a = CReaderT (ReaderT db m a)
  deriving (Monad, Functor, MonadTrans, Applicative, Alternative)

class (Monad m, Functor m, ConstraintDatabase db)
    => MonadCReader db m | m -> db where
  askDb :: m db
  localDb :: (db -> db) -> m a -> m a
  queryDb :: ConstraintQuery db r -> m (Set r)
  queryDb q = (query `liftM` askDb) `ap` return q

instance (Monad m, Functor m, ConstraintDatabase db)
    => MonadCReader db (CReaderT db m) where
  askDb = CReaderT ask
  localDb f (CReaderT m) = CReaderT $ local f m

instance MonadCReader r m => MonadCReader r (ListT m) where
  askDb   = lift askDb
  localDb = mapListT . localDb

instance MonadCReader r m => MonadCReader r (MaybeT m) where
  askDb   = lift askDb
  localDb = mapMaybeT . localDb

instance MonadCReader r m => MonadCReader r (EitherT e m) where
  askDb   = lift askDb
  localDb = mapEitherT . localDb

instance MonadCReader r m => MonadCReader r (ReaderT r' m) where
  askDb = lift askDb
  localDb = mapReaderT . localDb

instance MonadCReader r m => MonadCReader r (StateT s m) where
  askDb = lift askDb
  localDb = mapStateT . localDb

instance (MonadCReader r m, Monoid w) => MonadCReader r (WriterT w m) where
  askDb = lift askDb
  localDb = mapWriterT . localDb

