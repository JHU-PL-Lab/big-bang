/* Haskell side structure
ToHaskellObject
    Command
      RunCodeCommand
      ParseCommand
                                                
FromHaskellObject
    Response
      RunCodeResponse
      ParseResponse
    ProtocolError
*/


// java side structure
interface FromHaskellObject { int a = ...; ... }
abstract class Response implements FromHaskellObject { int b = ...; ... }
public class RunCodeResponse extends Response { int c = ...; ... }
public class ParseResponse extends Response { int d = ...; ... }
public class ProtocolError implements FromHaskellObject { int e = ...; ... }

/*
data FromHaskellObject
    = ResponseC Int Response
    | ProtocolErrorC Int ProtocolError

    data Response
    = RunCodeResponseC Int RunCodeResponse
    | ParseResponseC Int ParseResponse

    data RunCodeResponse
    = RunCodeResponse ...

    data ParseResponse
    = ParseResponse ...

    data ProtocolError
    = ProtocolError ...
*/

{"objectType":"ParseResponse", "fho": 1, "r": 2, "pr": 5}

instance FromJSON FromHaskellObject where
  parseJSON (Object obj) =
    let objectType = obj .: "objectType" in 
    case objectType of
      "ParseResponse" ->
        ResponseC <$> obj .: "a" <*> (ParseResponseC <$> obj .: "b" <*> (ParseResponse <$> obj .: "d"))
      "RunCodeResponse" ->
        ResponseC <$> obj .: "a" <*> (RunCodeResponseC <$> obj .: "b" <*> (RunCodeResponse <$> obj .: "d"))
