{-# LANGUAGE OverloadedStrings #-}

module Language.TinyBang.Communicator.ToHaskellObject where

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Map
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL

data ToHaskellObject = RunCodeCommand String Int
                  deriving Show

instance FromJSON ToHaskellObject where
  parseJSON (Object obj) = do
    supObj  <- obj .: "super" 
    RunCodeCommand <$> obj .: "usrInpStr" <*> supObj .: "cmdId"
    
  parseJSON _            = mzero

getInpStr :: ToHaskellObject -> String
getInpStr (RunCodeCommand inpStr i) = inpStr  


