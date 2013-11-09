{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DataKinds, KindSignatures, GADTs #-}

module Language.TinyBang.Communicator.FromHaskellObject where 

import Control.Applicative ((<$>), (<*>), empty)
import Data.Set
import Data.Map as DM
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL

import Language.TinyBang.Ast as AST
import Language.TinyBang.Interpreter as I
import Language.TinyBang.Syntax.Location 

data FromHaskellObject  
  = BatchModeErrorC BatchModeError
  | BatchModeResultC BatchModeResult--BMResultForJSON
    
data BatchModeError
  = BMProtocolFailure String
  | BMLexFailure String
  | BMParserFailure String
  -- | BMTypecheckFailure (TypecheckingError db)
  | BMEvalFailure EvalError [Clause]

data BatchModeResult
  = BatchModeResult
      -- |The variable containing the result of computation.
      FlowVar
      -- |The mapping from flow variables to their values.
      (Map FlowVar AST.Value)
      -- |The mapping from cell variables to their contents.
      (Map CellVar FlowVar)
  deriving (Eq, Ord, Show)

{-data BMResultForJSON
  = BMResultForJSON
      FlowVar
      (FlowVar, AST.Value)
      --(CellVar, FlowVar)
  deriving (Eq, Ord, Show)    
-}
-- | convert BatchModeResult to aeson readable format
--convertResultMapToTuple :: BatchModeResult -> BMResultForJSON
--convertResultMapToTuple (BatchModeResult flowVar mapFV mapCF) = BMResultForJSON flowVar (head . DM.toList $ mapFV) 

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

instance ToJSON BinaryOperator where
  toJSON (OpPlus origin) = object ["type" .= ("OpPlus" :: String), "origin" .= (toJSON origin)]
  toJSON (OpMinus origin) = object ["type" .= ("OpMinus" :: String), "origin" .= (toJSON origin)]
  toJSON (OpMult origin) = object ["type" .= ("OpMult" :: String), "origin" .= (toJSON origin)]
  toJSON (OpEqual origin) = object ["type" .= ("OpEqual" :: String), "origin" .= (toJSON origin)]
  toJSON (OpLess origin) = object ["type" .= ("OpLess" :: String), "origin" .= (toJSON origin)]
  toJSON (OpGreater origin) = object ["type" .= ("OpGreater" :: String), "origin" .= (toJSON origin)]

instance ToJSON Redex where
  toJSON (Define origin flowVar) = object ["type" .= ("Define" :: String), "origin" .= (toJSON origin), "flowVar" .= (toJSON flowVar)]
  toJSON (Appl origin flowVar flowVar2) = object ["type" .= ("Appl" :: String), "origin".= (toJSON origin), "flowVar" .= (toJSON flowVar), "flowVar2" .= (toJSON flowVar2)]
  toJSON (BinOp origin flowVar binOp flowVar2) = object ["type" .= ("BinOp" :: String), "origin".= (toJSON origin), "flowVar" .= (toJSON flowVar), "binOp" .= (toJSON binOp), "flowVar2" .= (toJSON flowVar2) ]

instance ToJSON LabelName where
  toJSON (LabelName origin str) = object ["type" .= ("LabelName" :: String), "origin" .= (toJSON origin), "nameStr" .= (toJSON str)]
  
instance ToJSON OnionOp where
  toJSON (OpOnionSub origin) = object ["type" .= ("OpOnionSub" :: String), "origin" .= (toJSON origin)]
  toJSON (OpOnionProj origin) = object ["type" .= ("OpOnionProj" :: String), "origin" .= (toJSON origin)]

instance ToJSON PrimitiveType where 
  toJSON (PrimInt origin) = object ["type" .= ("primInt" :: String), "origin" .= (toJSON origin)]
  toJSON (PrimChar origin) = object ["type" .= ("primChar" :: String), "origin" .= (toJSON origin)]
  
instance ToJSON InnerPattern where
  toJSON (PrimitivePattern origin primitiveType) = object ["type" .= ("PrimitivePattern" :: String), "origin" .= (toJSON origin), "primitiveType" .= (toJSON primitiveType)]
  toJSON (LabelPattern origin labelName cellVar innerPattern) = object ["type" .= ("LabelPattern" :: String), "origin" .= (toJSON origin), "labelName" .= (toJSON labelName), "cellVar" .= (toJSON cellVar), "innerPattern" .= (toJSON innerPattern)]
  toJSON (ConjunctionPattern origin innerPattern innerPattern2) = object ["type" .= ("ConjunctionPattern" :: String), "origin" .= (toJSON origin), "innerPattern" .= (toJSON innerPattern), "innerPattern2" .= (toJSON innerPattern2)]
  toJSON (ScapePattern origin) = object ["type" .= ("ScapePattern" :: String), "origin" .= (toJSON origin)]
  toJSON (EmptyOnionPattern origin) = object ["type" .= ("EmptyOnionPattern" :: String), "origin" .= (toJSON origin)]

instance ToJSON ProjectorTag where 
  toJSON ProjPrimTag = object ["type" .= ("ProjPrimTag" :: String)]
  toJSON ProjLabelTag = object ["type" .= ("ProjLabelTag" :: String)]
  toJSON ProjFunTag = object ["type" .= ("ProjFunTag" :: String)]
  
instance ToJSON (Projector tag ) where
  toJSON (ProjPrim origin primitiveType) = object ["type" .= ("ProjPrim" :: String), "origin" .= (toJSON origin), "primitiveType" .= (toJSON primitiveType)]
  toJSON (ProjLabel origin labelName) = object ["type" .= ("ProjLabel" :: String), "origin" .= (toJSON origin), "labelName" .= (toJSON labelName)]
  toJSON (ProjFun origin ) = object ["type" .= ("ProjFun" :: String), "origin" .= (toJSON origin)]
    
instance ToJSON AnyProjector where
  toJSON (SomeProjector projector) = object ["type" .= ("SomeProjector" :: String), "projector" .= (toJSON projector)]
  
instance ToJSON Pattern where
  toJSON (ValuePattern origin cellVar innerPattern) = object ["type" .= ("ValuePattern" :: String), "origin" .= (toJSON origin), "cellVar" .= (toJSON cellVar), "innerPattern" .= (toJSON innerPattern)]
  toJSON (ExnPattern origin cellVar innerPattern) = object ["type" .= ("ExnPattern" :: String), "origin" .= (toJSON origin), "cellVar" .= (toJSON cellVar), "innerPattern" .= (toJSON innerPattern)]
  
instance ToJSON Expr where 
  toJSON (Expr origin clauseLst) = object ["type" .= ("Expr" :: String), "origin" .= (toJSON origin), "clauseLst" .= (toJSON clauseLst)]
  
instance ToJSON AST.Value where
  toJSON (VInt origin i) = object ["type" .= ("VInt" :: String), "origin" .= (toJSON origin), "intVar" .= i]
  toJSON (VChar origin ch) = object ["type" .= ("VChar" :: String), "origin" .= (toJSON origin), "charVar" .= ch]
  toJSON (VEmptyOnion origin) = object ["type" .= ("VEmptyOnion" :: String), "origin" .= (toJSON origin)]
  toJSON (VLabel origin labelName cellVar) = object ["type" .= ("VLabel" :: String), "origin" .= (toJSON origin), "labelName" .= (toJSON labelName), "cellVar" .= (toJSON cellVar)]
  toJSON (VOnion origin flowVar flowVar2) = object ["type" .= ("Vonion" :: String), "origin" .= (toJSON origin), "flowVar" .= (toJSON flowVar), "flowVar2" .= (toJSON flowVar2)]
  toJSON (VOnionFilter origin flowVar onionOp anyProjector) = object ["type" .= ("VonionFilter" :: String), "origin" .= (toJSON origin), "flowVar" .= (toJSON flowVar), "onionOp" .= (toJSON onionOp), "anyProjector" .= (toJSON anyProjector)]
  toJSON (VScape origin pattern expr) = object ["type" .= ("VScape" :: String), "origin" .= (toJSON origin), "pattern" .= (toJSON pattern), "expr" .= (toJSON expr)]

instance ToJSON CellQualifier where
  toJSON (QualFinal origin) = object ["type" .= ("QualFinal" :: String), "origin" .= (toJSON origin)]
  toJSON (QualImmutable origin) = object ["type" .= ("QualImmutable" :: String), "origin" .= (toJSON origin)]
  toJSON (QualNone origin) = object ["type" .= ("QualNone" :: String), "origin" .= (toJSON origin)]

instance ToJSON FlowKind where
  toJSON FlowExn = object ["type" .= ("FlowExn" :: String)]
  
instance ToJSON EvaluatedClause where
  toJSON (ValueDef origin flowVar value) = object ["type" .= ("ValueDef" :: String), "origin" .= (toJSON origin), "flowVar" .= (toJSON flowVar), "value" .= (toJSON value)]
  toJSON (CellDef origin cellQualifier cellVar flowVar) = object ["type" .= ("CellDef" :: String), "origin" .= (toJSON origin), "cellQualifier" .= (toJSON cellQualifier), "cellVar" .= (toJSON cellVar), "flowVar" .= (toJSON flowVar)]
  toJSON (Flow origin flowVar flowKind flowVar2) = object ["type" .= ("Flow" :: String), "origin" .= (toJSON origin), "flowVar" .= (toJSON flowVar), "flowKind" .= (toJSON flowKind), "flowVar2" .= (toJSON flowVar2)]
  
instance ToJSON Clause where
  toJSON (RedexDef origin flowVar redex) = object ["type" .= ("RedexDef" :: String), "origin" .= (toJSON origin), "flowVar" .= (toJSON flowVar), "redex" .= (toJSON redex)]
  toJSON (CellSet origin flowVar cellVar) = object ["type" .= ("Cellset" :: String), "origin" .= (toJSON origin), "flowVar" .= (toJSON flowVar), "cellVar" .= (toJSON cellVar)]
  toJSON (CellGet origin flowVar cellVar) = object ["type" .= ("CellGet" :: String), "origin" .= (toJSON origin), "flowVar" .= (toJSON flowVar) , "cellVar" .= (toJSON cellVar)]
  toJSON (Throws origin flowVar flowVar2) = object ["type" .= ("Throws" :: String), "origin" .= (toJSON origin), "flowVar" .= (toJSON flowVar), "flowVar2" .= (toJSON flowVar2)]
  toJSON (Evaluated evalClause) = object ["type" .= ("Evaluated" :: String), "evalClause" .= (toJSON evalClause)]

instance ToJSON IllFormedness where
  toJSON (DuplicateFlowBinding flowVar) = object [ "type" .= ("DuplicateFlowBinding" :: String), "flowVar" .= (toJSON flowVar)]
  toJSON (DuplicateFlowUse flowVar) = object ["type" .= ("DuplicateFlowUse" :: String), "flowVar" .= (toJSON flowVar)]
  toJSON (DuplicateCellBinding cellVar) = object [ "type" .= ("DuplicateCellBinding" :: String), "cellVar" .= (toJSON cellVar)]
  toJSON (InvalidExpressionEnd clause) = object [ "type" .= ("InvalidExpressionEnd" :: String), "Clause" .= (toJSON clause)]
  toJSON (EmptyExpression) = object [ "type" .= ("EmptyExpression" :: String)]

-- | ToJSON instance for evalError
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
  toJSON (BatchModeErrorC (BMEvalFailure err clauseLst)) = object ["type" .= ("EvaluationFailure" :: String), "err" .= (toJSON err), "caluseLst" .= (toJSON clauseLst)]
  toJSON (BatchModeResultC (BatchModeResult flowVar flowMap cellMap)) = object ["type" .= ("BMResult" :: String), "flowVar" .= flowVar, "flowToValueMap" .= (toJSON $ DM.toList flowMap), "cellToFlowMap" .= (toJSON $ DM.toList cellMap)]
 -- toJSON (BatchModeResultC (BMResultForJSON flowVar (keyFlowVar, value))) = object ["type" .= ("BMResult" :: String), "flowVar" .= flowVar, "flowToValueMap" .= (toJSON (keyFlowVar, value))]  
