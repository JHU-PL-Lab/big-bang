{-# LANGUAGE OverloadedStrings #-}

module Language.TinyBang.Communicator.FromHaskellObject where 

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL

{-- class define --}

data FromHaskellObject = ResponseC { fho :: Int, sub_r :: Response }
                       | ProtocolErrorC { fho :: Int, sub_p :: ProtocolError }
                       deriving Show
                                
data Response = RunCodeResponseC { r :: Int, sub_rcr :: RunCodeResponse }
              | ParseResponseC { r :: Int,  sub_pr :: ParseResponse }
              deriving Show
                       
data RunCodeResponse = RunCodeResponse { rcr :: Int }
                     deriving Show
                              
data ParseResponse = ParseResponse { pr :: Int }
                   deriving Show
                          
data ProtocolError = ProtocolError { pe :: Int}
                   deriving (Show) 

{-- instance define --}
{-- expected parse structure: {"objectType":"ParseResponse", "fho": 1, "r": 2, "pr": 5} --}
instance FromJSON FromHaskellObject where
  parseJSON (Object obj) =
    (obj .: "objectType" :: Parser String) >>=
      (\typeStr ->
        case typeStr of      
          "RunCodeResponse" ->
            ResponseC <$> (obj .: "fho") <*> (RunCodeResponseC <$> (obj .: "r") <*> (RunCodeResponse <$> (obj .: "rcr")))
          "ParseResponse" ->
            ResponseC <$> (obj .: "fho") <*> (ParseResponseC <$> (obj .: "r") <*> (ParseResponse <$> (obj .: "pr")))
          "ProtocolError" ->
            ProtocolErrorC <$> (obj .: "fho") <*> (ProtocolError <$> (obj .: "pe")))
  parseJSON _             = empty
  
{-- expected class format: Just (ResponseC {fho = 1, sub_r = ParseResponseC {r = 2, sub_pr = ParseResponse {pr = 5}}}) --}
instance ToJSON FromHaskellObject where
  toJSON (ResponseC fho (RunCodeResponseC r (RunCodeResponse rcr))) = object [ "fho" .= fho, "r" .= r, "rcr" .= rcr, "objectType" .= ("RunCodeResponse" :: String)]
  toJSON (ResponseC fho (ParseResponseC r (ParseResponse pr))) = object [ "fho" .= fho, "r" .= r, "pr" .= pr, "objectType" .= ("ParseResponse" :: String)]
  toJSON (ProtocolErrorC fho (ProtocolError pe)) = object [ "fho" .= fho, "pe" .= pe, "objectType" .= ("ProtocolError" :: String)]

{-- main function for debug
main :: IO()
main = do  
  jsonStr <- getLine
  let obj = decode $ BL.pack jsonStr :: Maybe FromHaskellObject
  print obj
  let obj2 = ResponseC {fho = 1, sub_r = RunCodeResponseC {r = 2, sub_rcr = RunCodeResponse {rcr = 5}}}
  let reply = encode obj2
  BL.putStrLn reply    
--}

data ResultObject = RO {id :: Int, result :: String}

instance ToJSON ResultObject where 
  toJSON (RO i resultStr) = object ["id" .= i, "result" .= resultStr]
