{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |This module defines a type class and a number of default instances for
--  displaying human-readable forms of data.  This is distinct from the
--  Show typeclass in that the expected output is an informal representation
--  meant to be easy to parse rather than a Haskell expression which can be
--  used to reconstruct the data structure.
module Language.BigBang.Render.Display
( Display
, display
, displayList
, makeDoc
, makeListDoc
, indentSize
, module Text.PrettyPrint
) where

import Control.Monad (liftM)
import qualified Data.Set as Set
import Data.Set (Set)
import Language.Haskell.TH (mkName, Type(ConT))
import Text.PrettyPrint

-- |Defines the indentation used by the pretty printer
indentSize :: Int
indentSize = 4

-- |A simple function for displaying a list of elements.  This function will
--  adjust the appearance of the list based on its parameters.
makeDocForList :: (Display a)
               => (String -> [Doc] -> Doc) -> String -> [a] -> Doc
makeDocForList f s lst = makeDocForDocList f s $ map makeDoc lst

makeDocForDocList :: (String -> [Doc] -> Doc) -> String -> [Doc] -> Doc 
makeDocForDocList
        catF -- ^The function producing the document concatenator
        punc -- ^The punctuation to place between each document
        docs -- ^The documents to display
  = let dcat = catF $ render $ hcat docs in
    dcat $ punctuate (text punc) docs

makeCommaSeparatedDocForList :: (Display a) => [a] -> Doc
makeCommaSeparatedDocForList lst =
    makeDocForList catByComma ", " lst 

catByComma :: String -> [Doc] -> Doc
catByComma x = if elem ',' x then vcat else hcat

class Display a where
    display :: a -> String
    displayList :: [a] -> String
    makeDoc :: a -> Doc
    makeListDoc :: [a] -> Doc
    display = render . makeDoc
    displayList = render . makeListDoc
    makeListDoc = brackets . makeCommaSeparatedDocForList

instance Display Char where
    makeDoc = char
    makeListDoc = doubleQuotes . text

instance (Display a) => Display [a] where
    makeDoc = makeListDoc

instance (Display a) => Display (Set a) where
    makeDoc = braces . makeCommaSeparatedDocForList . Set.toList

instance (Display a, Display b) => Display (a,b) where
    makeDoc (a,b) = parens $ makeDocForDocList catByComma ", " docList
      where docList = [makeDoc a, makeDoc b]

$(
    let typeNames = ["Int","Integer","Float","Double"]
        showInstance n =
            [d|
                instance Display $(return $ ConT $ mkName n) where
                    makeDoc = text . show
             |]
    in liftM concat $ mapM showInstance typeNames
 )

