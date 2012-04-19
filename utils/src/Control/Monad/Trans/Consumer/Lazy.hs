-------------------------------------------------------------------------------
-- |
-- Module       : Control.Monad.Trans.Consumer.Lazy
--
-- A lazy monad for an environment with a consumable list (such as a series of
-- fresh tokens).
------------------------------------------------------------------------------

module Control.Monad.Trans.Consumer.Lazy
( Consumer
, runConsumer
, evalConsumer
, ConsumerT
, runConsumerT
, evalConsumerT
, next
, safeNext
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans

import Data.List.Utils (safeUnconcat)

type Consumer d = ConsumerT d Identity

runConsumer :: Consumer d a -> [d] -> (a, [d])
runConsumer c ds = runIdentity $ runConsumerT c ds

evalConsumer :: Consumer d a -> [d] -> a
evalConsumer c ds = runIdentity $ evalConsumerT c ds

newtype ConsumerT d m a = ConsumerT { runConsumerT :: [d] -> m (a, [d]) }

evalConsumerT :: (Monad m) => ConsumerT d m a -> [d] -> m a
evalConsumerT x ds = do
    ~(a, _) <- runConsumerT x $ ds
    return a

instance (Functor m) => Functor (ConsumerT s m) where
    fmap f m = ConsumerT $ \ds ->
        fmap (\ ~(a, ds') -> (f a, ds')) $ runConsumerT m ds

instance (Functor m, Monad m) => Applicative (ConsumerT s m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (ConsumerT d m) where
    return x = ConsumerT $ \ds -> return (x, ds)
    m >>= k  = ConsumerT $ \ds -> do
        ~(a, ds') <- runConsumerT m $ ds
        runConsumerT (k a) ds'
    fail str = ConsumerT $ \_ -> fail str

instance MonadTrans (ConsumerT s) where
    lift m = ConsumerT $ \ds -> do
        a <- m
        return (a, ds)

-- |Retrieve the next value from the consumption list.  If the list is empty,
--  an error results.
next :: (Monad m) => ConsumerT d m d
next = filteredNext $ const True

-- |Retrieves the next value from the consumption list if possible.
safeNext :: (Monad m) => ConsumerT d m (Maybe d)
safeNext = filteredSafeNext $ const True

-- |Retrieve the next value from the consumption list which matches the given
--  predicate.  If no such element exists, an error results.  The intervening
--  elements are consumed.
filteredNext :: (Monad m) => (d -> Bool) -> ConsumerT d m d
filteredNext f = ConsumerT $ \ds -> do
                        (x,xs) <- runConsumerT (filteredSafeNext f) ds
                        case x of
                            Just x' -> return (x',xs)
                            Nothing -> error "exhausted consumer list!"

-- |Retrieve the next value from the consumption list which matches the given
--  predicate (if possible).  The intervening elements are consumed.
filteredSafeNext :: (Monad m) => (d -> Bool) -> ConsumerT d m (Maybe d)
filteredSafeNext f = ConsumerT $ \ds ->
                        return $ safeUnconcat $ dropWhile (not . f) ds

