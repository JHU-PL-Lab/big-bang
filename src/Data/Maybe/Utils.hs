module Data.Maybe.Utils
( justIf
) where

-- |A utility function for producing a Maybe value based on a boolean.
justIf :: a -> Bool -> Maybe a
justIf v b = if b then Just v else Nothing