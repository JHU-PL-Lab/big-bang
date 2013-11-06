{-# LANGUAGE OverloadedStrings #-}

module Language.TinyBang.Communicator.FromHaskellObject where 

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Map
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL

import Language.TinyBang.Ast as AST
import Language.TinyBang.Interpreter as I

{-
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
  toJSON (ResponseC fho (ParseResponseC r (ParseResponse pr))) = object [ "fho" .= fho, "r" .= r, "pr" .= pr, "objectType" .= (" ParseResponse" :: String)]
  toJSON (ProtocolErrorC fho (ProtocolError pe)) = object [ "fho" .= fho, "pe" .= pe, "objectType" .= ("ProtocolError" :: String)]
-}

data FromHaskellObject  
  = BatchModeErrorC BatchModeError
  | BatchModeResultC BatchModeResult
    
data BatchModeError
  = BMInterpreterFailure String
  | BMLexFailure String
  | BMParserFailure String
  -- | BMTypecheckFailure (TypecheckingError db)
  | BMEvalFailure EvalError -- | [Clause]

data BatchModeResult
  = BatchModeResult
      -- |The variable containing the result of computation.
      FlowVar
      -- |The mapping from flow variables to their values.
      (Map FlowVar AST.Value)
      -- |The mapping from cell variables to their contents.
      (Map CellVar FlowVar)
  deriving (Eq, Ord, Show)

instance ToJSON FromHaskellObject where
  toJSON (BatchModeErrorC (BMLexFailure errStr)) = object  [ "type" .= ("LexerFailure" :: String),  "errMsg" .= errStr, "superType" .= (object ["type" .= ("BatchModeError" :: String)]) ]
  toJSON (BatchModeErrorC (BMParserFailure errStr)) = object  ["type" .= ("ParserError" :: String),  "errMsg" .= errStr, "superType" .= (object ["type" .= ("BatchModeError" :: String)]) ]
  toJSON (BatchModeErrorC (BMInterpreterFailure errStr)) = object  ["type" .= ("InterpreterError" :: String), "errMsg" .= errStr, "superType" .= (object ["type" .= ("BatchModeError" :: String)]) ]
--  toJSON (BatchModeErrorC (BMEvalFailure EvalError)) = object ["showEvalFailuer" .= ((show (BMEvalFailure EvalError)) :: String), "superType" .= (object ["type" .= ("BatchModeError" :: String)]) ]
--  toJSON BatchModeResult = object ["showResult" .= ((show BatchModeResult) :: String)]


















  








































