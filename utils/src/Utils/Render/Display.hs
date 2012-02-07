{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}

-- |This module defines a type class and a number of default instances for
--  displaying human-readable forms of data.  This is distinct from the
--  Show typeclass in that the expected output is an informal representation
--  meant to be easy to parse rather than a Haskell expression which can be
--  used to reconstruct the data structure.
module Utils.Render.Display
( Display
, display
, displayList
, makeDoc
, makeListDoc
, indentSize
, module Text.PrettyPrint
) where

import Control.Monad (liftM)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Language.Haskell.TH
import Text.PrettyPrint

-- |Defines the indentation used by the pretty printer
indentSize :: Int
indentSize = 4

makeDocForList :: (?debug :: Bool, Display a)
               => (String -> [Doc] -> Doc) -> String -> [a] -> Doc
makeDocForList = makeDocForListBy makeDoc

-- |A function for displaying a list of elements.  This function will
--  adjust the appearance of the list based on its parameters.
makeDocForListBy :: (?debug :: Bool)
                 => (a -> Doc)
                 -> (String -> [Doc] -> Doc)
                 -> String
                 -> [a]
                 -> Doc
makeDocForListBy toDoc f s lst = makeDocForDocList f s $ map toDoc lst

makeDocForDocList :: (?debug :: Bool)
                  => (String -> [Doc] -> Doc)
                  -> String -> [Doc] -> Doc
makeDocForDocList
        catF -- ^The function producing the document concatenator
        punc -- ^The punctuation to place between each document
        docs -- ^The documents to display
  = let dcat = catF $ render $ hcat docs in
    dcat $ punctuate (text punc) docs

makeCommaSeparatedDocForList :: (?debug :: Bool, Display a) => [a] -> Doc
makeCommaSeparatedDocForList lst =
    makeDocForList catByComma ", " lst

catByComma :: String -> [Doc] -> Doc
catByComma x = if elem ',' x then vcat else hcat

class Display a where
    display :: (?debug :: Bool) => a -> String
    displayList :: (?debug :: Bool) => [a] -> String
    makeDoc :: (?debug :: Bool) => a -> Doc
    makeListDoc :: (?debug :: Bool) => [a] -> Doc
    display = render . makeDoc
    displayList = render . makeListDoc
    makeListDoc = brackets . makeCommaSeparatedDocForList

displayMap :: (?debug :: Bool, Display k, Display v) =>
              (a -> [(k, v)]) -> a -> Doc
displayMap toList = braces . (makeDocForListBy mappingToDoc catByComma ", ")
             . toList
  where mappingToDoc (a,b) = makeDoc a <> char ':' <+> makeDoc b

instance Display Doc where
    makeDoc = id

instance Display Bool where
    makeDoc = text . show

instance Display Char where
    makeDoc = char
    makeListDoc = doubleQuotes . text

instance (Display a) => Display [a] where
    makeDoc = makeListDoc

instance (Display a) => Display (Set a) where
    makeDoc = braces . makeCommaSeparatedDocForList . Set.toList

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
                        char '(' <>
                        makeDocForDocList catByComma ", " $(return lstExpr) <>
                        char ')' |])
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
