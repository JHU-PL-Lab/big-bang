{-# LANGUAGE TemplateHaskell #-}

module Data.Adj
( Adj
, adjacent
) where

import Control.Applicative        ()
import Language.Haskell.TH.Syntax

-- |This typeclass indicates that the type, in addition to being totally
--  ordered, is equipped with the ability to determine whether or not two
--  elements are adjacent.  This is the case with spaces which are not
--  arbitrarily subdividable; the integers can implement Adj, but the rationals
--  cannot.  Elements are considered to be adjacent to themselves.
class (Ord a) => Adj a where
  adjacent :: a -> a -> Bool

enumAdjacent :: (Enum a, Eq a, Ord a) => a -> a -> Bool
enumAdjacent x y =
    let ix = fromEnum x
        iy = fromEnum y in
    ix - 1 == iy || ix == iy || ix + 1 == iy

$( return $
  let generateEnumAdjacentInstanceFor name =
        InstanceD [] (AppT (ConT ''Adj) $ ConT name)
          [FunD (mkName "adjacent") [Clause [] (NormalB $ VarE $ mkName "enumAdjacent") []]]
--            <$> [d|adjacent = enumAdjacent|]
  in map generateEnumAdjacentInstanceFor
        [   ''Bool
        ,   ''Char
        ,   ''Int
        ,   ''Integer
        ,   ''()
        ]
 )
