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
, module Text.PrettyPrint
) where

import Control.Monad (liftM)
import Data.List (intercalate)
import Language.Haskell.TH (mkName, Type(ConT))
import Text.PrettyPrint

class Display a where
    display :: a -> String
    displayList :: [a] -> String
    makeDoc :: a -> Doc
    makeListDoc :: [a] -> Doc
    display = render . makeDoc
    displayList = render . makeListDoc
    makeListDoc lst =
        lbrack <> (hcat $ punctuate (text ", ") $ map makeDoc lst) <> rbrack

instance Display Char where
    makeDoc = char
    makeListDoc = doubleQuotes . text

instance (Display a) => Display [a] where
    makeDoc = makeListDoc

$(
    let typeNames = ["Int","Integer","Float","Double"]
        showInstance n =
            [d|
                instance Display $(return $ ConT $ mkName n) where
                    makeDoc = text . show
             |]
    in liftM concat $ mapM showInstance typeNames
 )

