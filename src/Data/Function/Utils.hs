module Data.Function.Utils
( leastFixedPoint
) where

-- |A function to compute the least fixed point of a function over a value.
--  This function will continually invoke the function argument (starting with
--  the base value) until an invocation produces a value equal to the input.
leastFixedPoint :: (Eq a) => (a -> a) -> a -> a
leastFixedPoint f x =
    let x' = f x in
    if x == x' then x else leastFixedPoint f x'

