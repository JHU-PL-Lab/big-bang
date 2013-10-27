{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs, OverloadedStrings #-}

{--| temp toploop function for testing communicator function 
--}

module Language.TinyBang.ToploopTest where

import Data.Aeson
import Language.TinyBang.Communicator.FromHaskellObject as FHO
import qualified Data.ByteString.Lazy.Char8 as BL

genHSObj :: String -> ResultObject
genHSObj resultStr = FHO.RO 1 resultStr

genJsonStr :: ResultObject -> BL.ByteString
genJsonStr ro = encode ro
