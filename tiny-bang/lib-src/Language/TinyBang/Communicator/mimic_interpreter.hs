{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import FromHaskellObject
import ToHaskellObject

import Control.Monad
import Data.Char(toUpper)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import FromHaskellObject as FHO
import ToHaskellObject as TKO

{--
   1. Get jsonStr from java
   2. FromJSON(jsonStr) = ToHaskellObject
   3. Check the type of the ToHaskellObject got from step 2
        if objectType == "RunCodeCommand" 
        then Generate FromHaskellObject: RunCodeResponse
        elseif objectType == "ParseCommand"
      then Generate FromHaskellObject: ParseResponse
   4. Stdout[1] = toJSON(object got from step 3)
--}

{-- one time function, no need to review --}

main :: IO ()
main = do
  inpStr <- getLine 
  let toHSObj = decode $ BL.pack inpStr :: Maybe Object
  let replyObj =
        (toHSObj >>= (\inpObj -> flip parseMaybe inpObj $ \obj -> do
           objType <- (obj .: "objectType" :: Parser String)
           case objType of
             "RunCodeCommand" ->
               return $ ResponseC 1 . RunCodeResponseC 2 $ RunCodeResponse 3
             "ParseCommand" ->
               return $ ResponseC 1 . ParseResponseC 2 $ ParseResponse 4))
       
  BL.putStrLn (encode replyObj)

      
 
