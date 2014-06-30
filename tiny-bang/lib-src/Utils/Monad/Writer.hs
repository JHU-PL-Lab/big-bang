module Utils.Monad.Writer
( capture
) where

import Control.Monad.Writer

-- |Captures the content written by a given operation.  The writer activity
--  of that operation is silenced; it does not affect this monad's writer
--  results.  This is similar to running a simple writer; it is somewhat more
--  useful for monads created with @WriterT@.
capture :: (MonadWriter w m) => m a -> m (a, w)
capture = censor (const mempty) . listen
