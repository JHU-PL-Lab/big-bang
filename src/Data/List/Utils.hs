module Data.List.Utils
( safeHead
) where

-- TODO: deprecate in favor of Data.Maybe.listMaybe
-- |A function providing a safe head over lists.
safeHead :: [a] -> Maybe a
safeHead lst = case lst of
    h:t -> Just h
    [] -> Nothing


