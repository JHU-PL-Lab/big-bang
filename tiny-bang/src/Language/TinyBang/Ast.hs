{-# LANGUAGE TemplateHaskell #-}

{-|
  A module which defines the TinyBang A-normal form AST.
-}

module Language.TinyBang.Ast
( Expr(..)
, Clause(..)
, EvaluatedClause(..)
, Redex(..)
, Value(..)
, Pattern(..)
, InnerPattern(..)
, OnionOp(..)
, BinaryOperator(..)
, FlowKind(..)
, CellQualifier(..)
, Projector(..)
, PrimitiveType(..)
, LabelName(..)
, FlowVar(..)
, CellVar(..)
) where

import Control.Applicative ((<$>))

import Language.TinyBang.Syntax.Location
import Utils.Meta.Deriving

-- |A data type representing expressions.
data Expr
  = Expr SourceRegion [Clause]
  deriving (Show)

-- |A data type representing general clauses.
data Clause
  = RedexDef SourceRegion FlowVar Redex
  | CellSet SourceRegion CellVar FlowVar
  | CellGet SourceRegion FlowVar CellVar
  | Throws SourceRegion FlowVar FlowVar
  | Evaluated EvaluatedClause
  deriving (Show)

-- |A data type representing evaluated clauses.
data EvaluatedClause
  = ValueDef SourceRegion FlowVar Value
  | CellDef SourceRegion CellQualifier CellVar FlowVar
  | Flow SourceRegion FlowVar FlowKind FlowVar
  deriving (Show)

-- |A data type representing reducible expressions.
data Redex
  = Appl SourceRegion FlowVar FlowVar
  | BinOp SourceRegion FlowVar BinaryOperator FlowVar
  deriving (Show)

-- |A data type representing value forms.
data Value
  = VInt SourceRegion Integer
  | VChar SourceRegion Char
  | VEmptyOnion SourceRegion
  | VLabel SourceRegion LabelName CellVar
  | VOnion SourceRegion FlowVar FlowVar
  | VOnionFilter SourceRegion FlowVar OnionOp Projector
  | VScape SourceRegion Pattern Expr
  deriving (Show)

-- |A data type describing patterns.
data Pattern
  = ValuePattern SourceRegion CellVar InnerPattern
  | ExnPattern SourceRegion CellVar InnerPattern
  deriving (Show)

-- |A data type describing inner patterns.
data InnerPattern
  = PrimitivePattern SourceRegion PrimitiveType
  | LabelPattern SourceRegion LabelName CellVar InnerPattern
  | ConjunctionPattern SourceRegion InnerPattern InnerPattern
  | ScapePattern SourceRegion
  | EmptyOnionPattern SourceRegion
  deriving (Show)

-- |An enumeration of onion filter operators.
data OnionOp
  = OpOnionSub SourceRegion
  | OpOnionProj SourceRegion
  deriving (Show)

-- |An enumeration of binary value operators.
data BinaryOperator
  = OpPlus SourceRegion
  | OpMinus SourceRegion
  | OpEqual SourceRegion
  | OpLess SourceRegion
  | OpGreater SourceRegion
  deriving (Show)

-- |An enumeration of non-value flow kinds.
data FlowKind
  = FlowExn
  deriving (Eq,Ord,Show)

-- |An enumeration of cell qualifiers.
data CellQualifier
  = QualFinal SourceRegion
  | QualImmutable SourceRegion
  | QualNone SourceRegion
  deriving (Show)

-- |A data type for projectors.
data Projector
  = ProjPrim SourceRegion PrimitiveType
  | ProjLabel SourceRegion LabelName
  | ProjFun SourceRegion
  deriving (Show)

-- |A representation of primitive types.
data PrimitiveType
  = PrimInt SourceRegion
  | PrimChar SourceRegion
  deriving (Show)

-- |A semantic wrapper for label names.
data LabelName = LabelName SourceRegion String
  deriving (Show)

-- TODO: there is a bad overloading between "flow variable" and "flow kind"
-- discuss this

-- |A semantic wrapper for flow identifiers.
data FlowVar = FlowVar SourceRegion String
  deriving (Show)

-- |A semantic wrapper for cell identifiers.
data CellVar = CellVar SourceRegion String
  deriving (Show)

-- A series of Regioned declarations for the above types.

instance Regioned Expr where
  regionOf x = case x of
    Expr reg _ -> reg

instance Regioned Clause where
  regionOf x = case x of
    RedexDef reg _ _ -> reg
    CellSet reg _ _ -> reg
    CellGet reg _ _ -> reg
    Throws reg _ _ -> reg
    Evaluated c -> regionOf c

instance Regioned EvaluatedClause where
  regionOf x = case x of
    ValueDef reg _ _ -> reg
    CellDef reg _ _ _ -> reg
    Flow reg _ _ _ -> reg

instance Regioned Redex where
  regionOf x = case x of
    Appl reg _ _ -> reg
    BinOp reg _ _ _ -> reg

instance Regioned Value where
  regionOf x = case x of
    VInt reg _ -> reg
    VChar reg _ -> reg
    VEmptyOnion reg -> reg
    VLabel reg _ _ -> reg
    VOnion reg _ _ -> reg
    VOnionFilter reg _ _ _ -> reg
    VScape reg _ _ -> reg

instance Regioned Pattern where
  regionOf x = case x of
    ValuePattern reg _ _ -> reg
    ExnPattern reg _ _ -> reg

instance Regioned InnerPattern where
  regionOf x = case x of
    PrimitivePattern reg _ -> reg
    LabelPattern reg _ _ _ -> reg
    ConjunctionPattern reg _ _ -> reg
    ScapePattern reg -> reg
    EmptyOnionPattern reg -> reg

instance Regioned OnionOp where
  regionOf x = case x of
    OpOnionSub reg -> reg
    OpOnionProj reg -> reg

instance Regioned BinaryOperator where
  regionOf x = case x of
    OpPlus reg -> reg
    OpMinus reg -> reg
    OpEqual reg -> reg
    OpLess reg -> reg
    OpGreater reg -> reg

instance Regioned CellQualifier where
  regionOf x = case x of
    QualFinal reg -> reg
    QualImmutable reg -> reg
    QualNone reg -> reg

instance Regioned Projector where
  regionOf x = case x of
    ProjPrim reg _ -> reg
    ProjLabel reg _ -> reg
    ProjFun reg -> reg

instance Regioned PrimitiveType where
  regionOf x = case x of
    PrimInt reg -> reg
    PrimChar reg -> reg

instance Regioned LabelName where
  regionOf x = case x of
    LabelName reg _ -> reg

instance Regioned FlowVar where
  regionOf x = case x of
    FlowVar reg _ -> reg

instance Regioned CellVar where
  regionOf x = case x of
    CellVar reg _ -> reg
    
$(concat <$> sequence
  [ f name
  | f <- [deriveEqSkipFirst, deriveOrdSkipFirst]
  , name <-
      [ ''Expr
      , ''Clause
      , ''EvaluatedClause
      , ''Redex
      , ''Value
      , ''Pattern
      , ''InnerPattern
      , ''OnionOp
      , ''BinaryOperator
      , ''CellQualifier
      , ''Projector
      , ''PrimitiveType
      , ''LabelName
      , ''FlowVar
      , ''CellVar
      ]
  ])

