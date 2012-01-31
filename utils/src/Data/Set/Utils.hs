module Data.Set.Utils
( singIf
) where

import Data.Set (Set, singleton, empty)

singIf :: a -> Bool -> Set a
singIf item cond =
    if cond then singleton item else empty

