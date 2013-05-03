{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Concat
( Concatenatable(..)
) where

import GHC.Exts (maxTupleSize)

import Control.Applicative ((<$>))
import Language.Haskell.TH

class Concatenatable a b c where
  (+++) :: a -> b -> c
  
instance Concatenatable [a] [a] [a] where
  (+++) = (++)

-- Define instances of concatenation
$(
  let tupleInstanceSize = 15{-maxTupleSize-} in
  let tupleInstance :: Int -> Int -> Q [Dec]
      tupleInstance n m =
        let namesA = mkNames "a" n
            namesB = mkNames "b" m
            typeA = mkTupleType namesA
            typeB = mkTupleType namesB
            typeC = mkTupleType $ namesA ++ namesB
            nmA = mkName "a"
            nmB = mkName "b"
            expr = 
              LetE (map (\(nm,nms) -> ValD (TupP $ map VarP nms)
                                  (NormalB $ VarE nm) [])
                      [(nmA,namesA),(nmB,namesB)]) $
                TupE $ map VarE $ namesA ++ namesB
            defn = FunD (mkName "+++") [Clause [VarP nmA, VarP nmB]
                      (NormalB expr) []]
            inst = InstanceD []
                      (foldl AppT (ConT $ mkName "Concatenatable")
                        [typeA, typeB, typeC])
                      [defn]
        in
        return [inst]
        where
          mkNames :: String -> Int -> [Name]
          mkNames pfx count =
            map (mkName . (pfx++)) $ map show [1..count]
          mkTupleType names =
            foldl AppT (TupleT $ length names) (map VarT names)
  in
  concat <$> sequence
    [ tupleInstance n m
    | n <- [0..tupleInstanceSize]
    , m <- [0..tupleInstanceSize]
    , n+m <= maxTupleSize ]
 )
