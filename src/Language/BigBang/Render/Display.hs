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

class Display a where
    display :: a -> String
    displayList :: [a] -> String
    makeDoc :: a -> Doc
    makeListDoc :: [a] -> Doc
    display = render . makeDoc
    displayList = render . makeListDoc
    makeListDoc lst =
        brackets $ hcat $ punctuate (text ", ") $ map makeDoc lst

instance Display Char where
    makeDoc = char
    makeListDoc = doubleQuotes . text

instance (Display a) => Display [a] where
    makeDoc = makeListDoc

instance (Display a) => Display (Set a) where
    makeDoc set =
        braces $ hcat $ punctuate (text ", ") $ map makeDoc $ Set.toList set

$(
    let typeNames = ["Int","Integer","Float","Double"]
        showInstance n =
            [d|
                instance Display $(return $ ConT $ mkName n) where
                    makeDoc = text . show
             |]
    in liftM concat $ mapM showInstance typeNames
 )

