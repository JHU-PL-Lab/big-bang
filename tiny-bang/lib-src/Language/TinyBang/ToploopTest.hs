{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs, OverloadedStrings #-}

{--| temp toploop function for testing communicator function 
--}

module Language.TinyBang.ToploopTest where

import Data.Aeson
import Language.TinyBang.Communicator.ToHaskellObject as THO
import Language.TinyBang.Communicator.FromHaskellObject as FHO
import Language.TinyBang.Toploop as TL
import qualified Data.ByteString.Lazy.Char8 as BL

{-
genHSObj :: String -> ResultObject
genHSObj resultStr = FHO.RO 1 resultStr

genJsonStr :: ResultObject -> BL.ByteString
genJsonStr ro = encode ro
-}

messageHandler :: String -> String
messageHandler inpJsonStr = case (decode . BL.pack $ inpJsonStr) of
  Nothing -> BL.unpack (encode (FHO.ProtocolError 2 "Invalid Input"))   
  Just obj -> BL.unpack (encode (commandHandler obj))
                                         
                     
commandHandler :: ToHaskellObject -> FromHaskellObject
commandHandler cmdObj = FHO.Response 1 resultStr
  where usrInpSrc = getInpStr cmdObj
        resultStr = runCodeCommand usrInpSrc

runCodeCommand :: String -> String
runCodeCommand inpSrc = TL.stringyInterpretSource config inpSrc
  where config = InterpreterConfiguration
                   { typechecking = False
                   , evaluating = True
                   , databaseType = Simple }
                   
{-
messageHandler :: String -> String
messageHandler inp =
    case fromJSON str of
        Nothing -> toJSON $ ErrorC $ ProtocolErrorC $ UnrecognizedCommand str
        Just obj -> toJSON $ commandHandler obj

    case cmdObj of
        RunCodeCommandC (RunCodeCommand { c = requestId, sub_rcc = RunCodeCommand { code = src, filename = filename }}) ->
            case runCodeCommand filename src of
                Left runCodeError ->
                    case runCodeError of
                        RunCodeLexError msg -> ErrorResponseC $ ErrorResponse { err = runCodeError }
                Right (env,var) -> RunCodeResponseC ...
    
runCodeCommand :: String -> String -> Either RunCodeError (EvalEnv, FlowVar)
runCodeCommand filename src = do
    tokens <- doStep RunCodeLexError $ lexTinyBang filename src
    ast <- doStep RunCodeParseError $ parseTinyBang tokens
    doStep RunCodeEvalError $ eval ast
    where
        doStep :: (a -> RunCodeError) -> Either a b -> Either RunCodeError b
        doStep convert value =
            case value of
                Left v -> convert v
                Right v -> Right v
-}
