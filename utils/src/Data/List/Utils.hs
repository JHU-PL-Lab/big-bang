module Data.List.Utils
( safeUnconcat
) where

-- |Given a list, destructs it into a safe head and the remainder of the list.
--  If the list was empty, no head is produced and so the remainder is still the
--  empty list.
safeUnconcat :: [a] -> (Maybe a, [a])
safeUnconcat xs = case xs of
  []    -> (Nothing,[])
  x:xs' -> (Just x, xs')