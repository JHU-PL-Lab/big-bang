{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}

-- |This module defines a type class and a number of default instances for
--  displaying human-readable forms of data.  This is distinct from the
--  Show typeclass in that the expected output is an informal representation
--  meant to be easy to parse rather than a Haskell expression which can be
--  used to reconstruct the data structure.
module Language.TinyBang.Display
( Display
, display
, displayList
, makeDoc
, makeListDoc
, indentSize
, module Text.PrettyPrint.Leijen
, delimSepDoc
, sepDoc
, binaryOpDoc
, denseDisplay
) where

import Control.Monad (liftM)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Language.Haskell.TH
import Text.PrettyPrint.Leijen hiding ((<$>),list)

import Data.Adj

-- |Defines the indentation used by the pretty printer
indentSize :: Int
indentSize = 4

-- |A generic rendering function.
render :: Doc -> String
render x = displayS (renderPretty 1.0 80 x) ""
-- TODO: make the rendering function capable of understanding terminal width

-- |A typeclass for displayable types.  This typeclass is distinct from @Show@
--  in that it presents a human-friendly and potentially *incomplete*
--  representation of the data.
class Display a where
    display :: a -> String
    displayList :: [a] -> String
    makeDoc :: a -> Doc
    makeListDoc :: [a] -> Doc
    display = render . makeDoc
    displayList = render . makeListDoc
    makeListDoc = delimSepDoc lbracket rbracket comma . map makeDoc
    displayMap :: (Display k, Display v) => (a -> [(k, v)]) -> a -> Doc
    displayMap toList =
        delimSepDoc lbrace rbrace comma . map mappingToDoc . toList
      where mappingToDoc (k,v) = makeDoc k <> char ':' <+> makeDoc v

---- Utility functions ------------------------------------

-- |Defines a utility function for loose concatenation of elements.  The given
--  list will be concatenated by a separator and surrounded in delimiters such
--  that (1) the elements will either be on different lines or the same line,
--  (2) the elements will be aligned, and (3) the left delimiter will appear to
--  the left of the alignment.  This is similar to @encloseSep@ but makes
--  different decisions regarding delimiter placement and indentation.
delimSepDoc :: Doc -> Doc -> Doc -> [Doc] -> Doc
delimSepDoc l r delim xs =
  let ds =
        if null xs
          then []
          else (map (<> delim) $ init xs) ++ [last xs]
  in
  l <> group (align (vsep ds)) <> r

-- |A simpler version of @delimSepDoc@ which uses empty delimiters.  This is
--  still useful for grouping elements by a delimiter and having the all-or-
--  nothing newline property.
sepDoc :: Doc -> [Doc] -> Doc
sepDoc = delimSepDoc empty empty

-- |A binary version of @sepDoc@ which does makeDoc translation.
binaryOpDoc :: (Display x, Display y, Display z) => x -> y -> z -> Doc
binaryOpDoc x y z =
  group (nest indentSize (vsep [makeDoc x, makeDoc y, makeDoc z]))

-- |Defines a utility function for creating a list of documents over an
--  ordered type equipped with adjacency.  This function is used to transform a
--  dense list (such as [1,2,3,4,5,7,8]) into a more manageable one (such as
--  [1..5,7,8]) by joining adjacent sequences of three or more elements into
--  a single document.  The @etc@ argument is used whenever documents have been
--  dropped (as ".." is above).
denseDisplay :: (Display a, Adj a, Ord a)
             => Doc -> [a] -> [Doc]
denseDisplay etc elems =
  let xs' = reduce $ zip elems elems in
  concat $ map docify xs'
  where reduce :: (Display a, Adj a, Ord a) => [(a,a)] -> [(a,a)]
        reduce xs = case xs of
                      [] -> xs
                      _:[] -> xs
                      (a,b):(c,d):t | adjacent b c -> reduce $ (a,d):t
                      (a,b):(c,d):t -> (a,b):(reduce $ (c,d):t)
        docify :: (Display a, Adj a, Ord a) => (a,a) -> [Doc]
        docify (a,b) =
          if a == b
            then [makeDoc a]
            else if adjacent a b
                 then [makeDoc a, makeDoc b]
                 else [makeDoc a <> etc <> makeDoc b]

---- Some basic display instances -------------------------

instance Display Doc where
    makeDoc = id

instance Display Bool where
    makeDoc = text . show

instance Display Char where
    makeDoc = char
    makeListDoc = dquotes . text

instance (Display a) => Display [a] where
    makeDoc = makeListDoc

instance (Display a) => Display (Set a) where
    makeDoc = delimSepDoc lbrace rbrace comma . map makeDoc . Set.toList

instance (Display a) => Display (Maybe a) where
    makeDoc = maybe (text "Nothing") ((text "Just" <+>) . makeDoc)

instance (Display a, Display b) => Display (Either a b) where
    makeDoc = either makeDoc makeDoc

instance (Display k, Display v) => Display (Map k v) where
    makeDoc = displayMap Map.toList
instance (Display v) => Display (IntMap v) where
    makeDoc = displayMap IntMap.toList

$(
    let typeNames = ["Int","Integer","Float","Double"]
        showInstance n =
            [d|
                instance Display $(return $ ConT $ mkName n) where
                    makeDoc = text . show
             |]
    in liftM concat $ mapM showInstance typeNames
 )

$(
    let showInstance n =
            let tvars = map ("a"++) $ map show $ [0..(n-1)]
                context = map
                    (\nm -> ClassP (mkName "Display") [VarT $ mkName nm]) tvars
                tAppNm t s = AppT t $ VarT $ mkName s
                typ = foldl tAppNm (TupleT n) tvars
                params = map (VarP . mkName) tvars
                lstExpr = ListE $ map
                    (\nm -> AppE (VarE $ mkName "makeDoc") (VarE $ mkName nm))
                    tvars
                makeDocFunClauses = [clause ([tupP $ map return params])
                    (normalB [|
                        delimSepDoc lparen rparen comma $(return lstExpr)
                        |])
                    []]
                makeDocFun = funD (mkName "makeDoc") makeDocFunClauses
                decl = instanceD (return context)
                    (appT (conT $ mkName "Display") $ return typ)
                    [makeDocFun]
            in
            decl
    in
    mapM showInstance [2..9]
 )


