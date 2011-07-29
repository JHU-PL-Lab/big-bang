{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |This module defines a type class and a number of default instances for
--  displaying human-readable forms of data.  This is distinct from the
--  Show typeclass in that the expected output is an informal representation
--  meant to be easy to parse rather than a Haskell expression which can be
--  used to reconstruct the data structure.
module Language.BigBang.Render.Display
( Display, display, displayList
) where

import Control.Monad (liftM)
import Data.List (intercalate)
import Language.Haskell.TH (mkName, Type(ConT))

class Display a where
    display :: a -> String
    displayList :: [a] -> String
    displayList lst =
        "[" ++ (intercalate ", " (map display lst)) ++ "]"

instance Display Char where
    display = show
    displayList lst = "\"" ++ lst ++ "\""

instance (Display a) => Display [a] where
    display = displayList

$(
    let typeNames = ["Int","Integer","Float","Double"]
        showInstance n =
            [d|
                instance Display $(return $ ConT $ mkName n) where
                    display = show
             |]
    in liftM concat $ mapM showInstance typeNames
 )

