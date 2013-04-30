{-|
  A module containing utilities for working with monads.
-}

module Utils.Monad
( ifM
) where

-- |A monadic if-then-else.
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM condM t f = do
  cond <- condM
  if cond then t else f
