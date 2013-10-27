{-# LANGUAGE OverloadedStrings #-}

module Language.TinyBang.Communicator.ToHaskellObject where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL

{-- class define --}
data ToHaskellObject = CommandC {tho :: Int, sub_c :: Command }
                      deriving Show

data Command = RunCodeCommandC {c :: Int, sub_rcc :: RunCodeCommand }
             | ParseCommandC {c :: Int, sub_pc :: ParseCommand }
             deriving Show

data RunCodeCommand = RunCodeCommand {rcc :: Int}
                    deriving Show

data ParseCommand = ParseCommand {pc :: Int}
                  deriving Show
                           
{-- instance define --}
{-- expected json format {"objectType":"RunCodeCommand", "c":1, "rcc":2} --}

instance FromJSON ToHaskellObject where
  parseJSON (Object obj) =
    (obj .: "objectType" :: Parser String) >>=
      (\typeStr ->
        case typeStr of      
          "RunCodeCommand" ->
            CommandC <$> (obj .: "tho") <*> (RunCodeCommandC <$> (obj .: "c") <*> (RunCodeCommand <$> (obj .: "rcc")))
          "ParseCommand" ->
            CommandC <$> (obj .: "tho") <*> (ParseCommandC <$> (obj .: "c") <*> (ParseCommand <$> (obj .: "pc"))))
  parseJSON _            = empty

{-- expected object format Command {c = 1, sub_pc = ParseCommand {p = 2}} --}
  
instance ToJSON ToHaskellObject where
  toJSON (CommandC tho (RunCodeCommandC c (RunCodeCommand rcc))) =
    object [ "tho" .= tho, "c" .= c, "rcc" .= rcc, "objectType" .= ("RunCodeCommand" :: String)]
  toJSON (CommandC tho (ParseCommandC c (ParseCommand pc))) =
    object [ "tho" .= tho, "c" .= c, "pc" .= pc, "objectType" .= ("ParseCommand" :: String)]

