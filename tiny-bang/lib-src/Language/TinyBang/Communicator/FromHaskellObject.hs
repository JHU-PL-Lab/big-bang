{-# LANGUAGE OverloadedStrings #-}

module Language.TinyBang.Communicator.FromHaskellObject where 

import Control.Applicative ((<$>), (<*>), empty)
import Data.Set
import Data.Map
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL

import Language.TinyBang.Ast as AST
import Language.TinyBang.Interpreter as I
import Language.TinyBang.Syntax.Location 

{-

data EvalError
  = IllFormedExpression IllFormedness
  | OpenExpression (Set AnyVar)
  | FlowVarNotClosed FlowVar
  | CellVarNotClosed CellVar
  --| ProjectionFailure FlowVar AnyProjector
  | ApplicationFailure FlowVar FlowVar
  deriving (Eq, Ord, Show)

/Ast/WellFormedness.hs
data IllFormedness
  = DuplicateFlowBinding FlowVar
  | DuplicateFlowUse FlowVar
  | DuplicateCellBinding CellVar
  --| InvalidExpressionEnd Clause
  | EmptyExpression
  deriving (Eq, Ord, Show)

/Ast/Data.hs
data AnyVar
  = SomeFlowVar FlowVar
  | SomeCellVar CellVar
  deriving (Eq, Ord, Show)

data FlowVar
  = FlowVar Origin String
  | GenFlowVar Origin String Integer
  deriving (Show)

data CellVar
  = CellVar Origin String
  | GenCellVar Origin String Integer
  deriving (Show)

data Origin
  = SourceOrigin SourceRegion
    -- ^ Signifies an AST node which originates from source code.

| ComputedOrigin [Origin]
    -- ^ Signifies an AST node which originates from the computation of other
    --   nodes.  The list contains the origins of the nodes involved in the
    --   computation.
  deriving (Eq, Ord, Show)

/syntax/location.hs
-- |Defines a region of source code.
data SourceRegion
  = SourceRegion -- ^ Signifies a source region over known locations
      SourceLocation -- ^ The start position
      SourceLocation -- ^ The end position
  deriving (Eq, Ord, Show)
  
-- |Defines a data type for text positions.  The @lineNo@ is one-based; the
--  @colNo@ is zero-based.
data SourceLocation
  = TextSource
      { textSourceDocument :: SourceDocument
      , textSourceLineNo :: Int
      , textSourceColNo :: Int }
  | Unknown
  deriving (Eq, Ord, Show)
  
-- |Defines a data type describing source code documents.  A source document is
--  some resource which contains the source code in textual form.
data SourceDocument
  = UnknownDocument
  deriving (Eq, Ord, Show)
  
-}
  
data FromHaskellObject  
  = BatchModeErrorC BatchModeError
  | BatchModeResultC BatchModeResult
    
data BatchModeError
  = BMProtocolFailure String
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

{-  TODO implement toJSON for Clause
-}
-- | elementary data for FlowVarNotClosed CellVarNotClosed and ApplicationFailure
instance ToJSON SourceDocument where
  toJSON UnknownDocument = object ["type" .= ("SourceDocument" :: String), "SourceDocument" .= ("UnknownDocument" :: String)]

instance ToJSON SourceLocation where
  toJSON (TextSource {textSourceDocument = srcDoc, textSourceLineNo = lN, textSourceColNo = cN }) = object ["type" .= ("TextSource" :: String), "textSourceDocument" .= (toJSON srcDoc), "textSourceLineNo" .= lN, "textSourceColNo" .= cN]

instance ToJSON SourceRegion where
  toJSON (SourceRegion initLoc endLoc) = object ["type" .= ("SourceRegion" :: String), "StartPosition" .= (toJSON initLoc), "EndPosition" .= (toJSON endLoc)] 

instance ToJSON Origin where
  toJSON (SourceOrigin srcRegion) = object ["type" .= ("SourceOrigin" :: String), "SourceOrigin" .= (toJSON srcRegion)]
  toJSON (ComputedOrigin origLst) = object ["type" .= ("ComputedOrigin" :: String), "ComputedOrigin" .= (toJSON origLst)]

instance ToJSON FlowVar where
  toJSON (FlowVar orig contentsStr) = object [ "type" .= ("FlowVar" :: String), "Origin" .= (toJSON orig), "FlowContents" .= contentsStr ]
  toJSON (GenFlowVar orig contentsStr i) = object [ "type".= ("GenFlowVar" :: String), "Origin" .= (toJSON orig), "FlowContents" .= contentsStr, "cellNum" .= i ]

instance ToJSON CellVar where
  toJSON (CellVar orig contentsStr) = object [ "type" .= ("CellVar" :: String), "Origin" .= (toJSON orig), "FlowContents" .= contentsStr]
  toJSON (GenCellVar orig contentsStr i) = object [ "type".= ("GenCellVar" :: String), "Origin" .= (toJSON orig), "FlowContents" .= contentsStr, "cellNum" .= i ]

instance ToJSON AnyVar where
  toJSON (SomeFlowVar flowVar) = object [ "type" .= ("SomeFlowVar" :: String), "flowVar" .= (toJSON flowVar)]
  toJSON (SomeCellVar cellVar) = object [ "type" .= ("SomeCellVar" :: String), "cellVar" .= (toJSON cellVar)]

instance ToJSON IllFormedness where
  toJSON (DuplicateFlowBinding flowVar) = object [ "type" .= ("DuplicateFlowBinding" :: String), "flowVar" .= (toJSON flowVar)]
  toJSON (DuplicateFlowUse flowVar) = object ["type" .= ("DuplicateFlowUse" :: String), "flowVar" .= (toJSON flowVar)]
  toJSON (DuplicateCellBinding cellVar) = object [ "type" .= ("DuplicateCellBinding" :: String), "cellVar" .= (toJSON cellVar)]
  -- toJSON (InvalidExpressionEnd clause) = object [ "type" .= ("InvalidExpressionEnd" :: String), "Clause" .= (toJSON clause)]
  toJSON (EmptyExpression) = object [ "type" .= ("EmptyExpression" :: String)]

-- | Main toJSON instance for evalError
instance ToJSON EvalError where
  toJSON (OpenExpression varSet) = object ["type" .= ("OpenExpression" :: String), "VarSet" .= (toJSON varSet)]
  toJSON (FlowVarNotClosed flowVar) = object [ "type" .= ("FlowVarNotClosedError" :: String), "FlowVar" .= (toJSON flowVar) ]
  toJSON (CellVarNotClosed cellVar) = object [ "type" .= ("CellVarNotClosedError" :: String), "CellVar" .= (toJSON cellVar) ]
  toJSON (ApplicationFailure flowVar1 flowVar2) = object [ "type" .= ("ApplicationFailure" :: String), "FlowVar1" .= (toJSON flowVar1), "FlowVar2" .= (toJSON flowVar2) ]

-- | toJSON instance for FromHaskellObject
instance ToJSON FromHaskellObject where
  toJSON (BatchModeErrorC (BMLexFailure errStr)) = object  [ "type" .= ("LexerFailure" :: String),  "errMsg" .= errStr, "superType" .= (object ["type" .= ("BatchModeError" :: String)]) ]
  toJSON (BatchModeErrorC (BMParserFailure errStr)) = object  ["type" .= ("ParserError" :: String),  "errMsg" .= errStr, "superType" .= (object ["type" .= ("BatchModeError" :: String)]) ]
  toJSON (BatchModeErrorC (BMProtocolFailure errStr)) = object  ["type" .= ("ProtocolError" :: String), "errMsg" .= errStr, "superType" .= (object ["type" .= ("BatchModeError" :: String)]) ]
  toJSON (BatchModeErrorC (BMEvalFailure err)) = object ["type" .= ("EvaluationFailure" :: String), "err" .= (toJSON err)]
