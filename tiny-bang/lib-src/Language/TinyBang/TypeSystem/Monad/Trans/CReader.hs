{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Language.TinyBang.TypeSystem.Monad.Trans.CReader
( CReaderT
, MonadCReader(..)
, CReader
, runCReader
, runCReaderT
, askLowerBounds
)
where

import Control.Applicative (Applicative, Alternative, (<$>), (<*>), pure)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.List (ListT, mapListT)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Data.Set (Set)

import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.Types

type CReader db a = CReaderT db Identity a

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

askLowerBounds :: (Applicative m, MonadCReader db m)
               => FlowTVar -> m (Set (Type db))
askLowerBounds a = getLowerBounds <$> askDb <*> pure a
