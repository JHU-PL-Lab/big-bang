{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Language.PatBang.TypeSystem.Monad.Trans.CReader
( CReaderT
, MonadCReader(..)
, CReader
, runCReader
, runCReaderT
)
where

import Control.Applicative
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.List (ListT, mapListT)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)

import Language.PatBang.TypeSystem.ConstraintDatabase

type CReader db = CReaderT db Identity

runCReader :: ConstraintDatabase db => CReader db a -> db -> a
runCReader m = runIdentity . runCReaderT m

runCReaderT :: ConstraintDatabase db => CReaderT db m a -> db -> m a
runCReaderT (CReaderT m) = runReaderT m

newtype CReaderT db m a = CReaderT (ReaderT db m a)
  deriving (Monad, Functor, MonadTrans, Applicative, Alternative)

class (Monad m, ConstraintDatabase db) => MonadCReader db m | m -> db where
  askDb :: m db
  localDb :: (db -> db) -> m a -> m a

instance (Monad m, ConstraintDatabase db) => MonadCReader db (CReaderT db m) where
  askDb = CReaderT ask
  localDb f (CReaderT m) = CReaderT $ local f m

instance MonadCReader r m => MonadCReader r (ListT m) where
    askDb   = lift askDb
    localDb = mapListT . localDb
    --readerDb = lift . readerDb

instance MonadCReader r m => MonadCReader r (MaybeT m) where
    askDb   = lift askDb
    localDb = mapMaybeT . localDb
    --readerDb = lift . readerDb

instance MonadCReader r m => MonadCReader r (EitherT e m) where
    askDb   = lift askDb
    localDb = mapEitherT . localDb
