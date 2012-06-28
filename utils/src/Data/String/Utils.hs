{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}

-- |This module contains utility functions for strings.
module Data.String.Utils
( numericSuperscript
) where

-- |This function will superscript all digits which appear in a given string.
numericSuperscript :: String -> String
numericSuperscript = map f
  where f c =
          case c of
            '0' -> '\x2070'
            '1' -> '\x00b9'
            '2' -> '\x00b2'
            '3' -> '\x00b3'
            '4' -> '\x2074'
            '5' -> '\x2075'
            '6' -> '\x2076'
            '7' -> '\x2077'
            '8' -> '\x2078'
            '9' -> '\x2079'
            _ -> c

