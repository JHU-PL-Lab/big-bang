module Utils.Monad.Read
( readMaybe
) where

import Data.Maybe

-- |A convenient wrapper around @reads@
readMaybe :: (Read a) => String -> Maybe a
readMaybe = listToMaybe . map fst . reads
