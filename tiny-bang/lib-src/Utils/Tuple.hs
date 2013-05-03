{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Utils.Tuple
( tupleApply
, ($$$)
) where

import GHC.Exts (maxTupleSize)

import Control.Applicative ((<$>))
import Language.Haskell.TH

import Utils.TemplateHaskell

class TupleApplication a b c where
  tupleApply :: a -> b -> c
  ($$$) :: a -> b -> c
  ($$$) = tupleApply
  infixl 2 $$$
  
instance TupleApplication (() -> a) () a where
  tupleApply f = f
  
instance TupleApplication (a -> b) a b where
  tupleApply = ($)
  
$(
  let tupleApplInst :: Int -> Q [Dec]
      tupleApplInst n =
        let names = mkNames "x" n
            fname = mkName "f"
            expr = foldl AppE (VarE fname) $ map VarE names
            defn = FunD (mkName "tupleApply")
                    [Clause [VarP fname, TupP $ map VarP names]
                    (NormalB expr) []]
            rname = mkName "r"
            typeA = foldr1 (\a b -> AppT (AppT ArrowT a) b) $ map VarT $
                      names ++ [rname]
            typeB = foldl AppT (TupleT (length names)) $ map VarT names
            typeC = VarT rname
            inst = InstanceD [] (foldl AppT (ConT $ mkName "TupleApplication")
                                  [typeA,typeB,typeC]) [defn]
        in
        return [inst]
  in
  concat <$> sequence [tupleApplInst n | n <- [2..maxTupleSize]]
 )