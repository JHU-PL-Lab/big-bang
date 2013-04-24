{-# LANGUAGE TemplateHaskell #-}

{-|
  A module which defines the data structures which comprise the TinyBang ANF
  AST.
-}

module Language.TinyBang.Ast.Data
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
, Origin(..)
, unLabelName
, unFlowVar
, unCellVar
, projPrim
, projLabel
, projFun
, primInt
, primChar
) where

import Control.Applicative ((<$>))

import Language.TinyBang.Syntax.Location
import Utils.Meta.Deriving

-- |A data type representing expressions.
data Expr
  = Expr Origin [Clause]
  deriving (Show)

-- |A data type representing general clauses.
data Clause
  = RedexDef Origin FlowVar Redex
  | CellSet Origin CellVar FlowVar
  | CellGet Origin FlowVar CellVar
  | Throws Origin FlowVar FlowVar
  | Evaluated EvaluatedClause
  deriving (Show)

-- |A data type representing evaluated clauses.
data EvaluatedClause
  = ValueDef Origin FlowVar Value
  | CellDef Origin CellQualifier CellVar FlowVar
  | Flow Origin FlowVar FlowKind FlowVar
  deriving (Show)

-- |A data type representing reducible expressions.
data Redex
  = Define Origin FlowVar
  | Appl Origin FlowVar FlowVar
  | BinOp Origin FlowVar BinaryOperator FlowVar
  deriving (Show)

-- |A data type representing value forms.
data Value
  = VInt Origin Integer
  | VChar Origin Char
  | VEmptyOnion Origin
  | VLabel Origin LabelName CellVar
  | VOnion Origin FlowVar FlowVar
  | VOnionFilter Origin FlowVar OnionOp Projector
  | VScape Origin Pattern Expr
  deriving (Show)

-- |A data type describing patterns.
data Pattern
  = ValuePattern Origin CellVar InnerPattern
  | ExnPattern Origin CellVar InnerPattern
  deriving (Show)

-- |A data type describing inner patterns.
data InnerPattern
  = PrimitivePattern Origin PrimitiveType
  | LabelPattern Origin LabelName CellVar InnerPattern
  | ConjunctionPattern Origin InnerPattern InnerPattern
  | ScapePattern Origin
  | EmptyOnionPattern Origin
  deriving (Show)

-- |An enumeration of onion filter operators.
data OnionOp
  = OpOnionSub Origin
  | OpOnionProj Origin
  deriving (Show)

-- |An enumeration of binary value operators.
data BinaryOperator
  = OpPlus Origin
  | OpMinus Origin
  | OpEqual Origin
  | OpLess Origin
  | OpGreater Origin
  deriving (Show)

-- |An enumeration of non-value flow kinds.
data FlowKind
  = FlowExn
  deriving (Eq,Ord,Show)

-- |An enumeration of cell qualifiers.
data CellQualifier
  = QualFinal Origin
  | QualImmutable Origin
  | QualNone Origin
  deriving (Show)

-- |A data type for projectors.
data Projector
  = ProjPrim Origin PrimitiveType
  | ProjLabel Origin LabelName
  | ProjFun Origin
  deriving (Show)

-- |A representation of primitive types.
data PrimitiveType
  = PrimInt Origin
  | PrimChar Origin
  deriving (Show)

-- |A semantic wrapper for label names.
data LabelName
  = LabelName Origin String
  deriving (Show)

-- |A semantic wrapper for flow identifiers.
data FlowVar
  = FlowVar Origin String
  | GenFlowVar Origin String Integer
  deriving (Show)

-- |A semantic wrapper for cell identifiers.
data CellVar
  = CellVar Origin String
  | GenCellVar Origin String Integer
  deriving (Show)
  
-- |A data structure describing source origin.
data Origin
  = SourceOrigin SourceRegion
    -- ^ Signifies an AST node which originates from source code.
  | ComputedOrigin [Origin]
    -- ^ Signifies an AST node which originates from the computation of other
    --   nodes.  The list contains the origins of the nodes involved in the
    --   computation.
  deriving (Eq, Ord, Show)
  
-- Destructors for the above data types

genSeparator :: String
genSeparator = "__"

unLabelName :: LabelName -> String
unLabelName (LabelName _ s) = s

unFlowVar :: FlowVar -> String
unFlowVar x = case x of
  FlowVar _ s -> s
  GenFlowVar _ s n -> s ++ genSeparator ++ show n
  
unCellVar :: CellVar -> String
unCellVar y = case y of
  CellVar _ s -> s
  GenCellVar _ s n -> s ++ genSeparator ++ show n
  
-- Smart constructors for the above data types

generated :: Origin
generated = ComputedOrigin []

projPrim :: PrimitiveType -> Projector
projPrim = ProjPrim generated

projLabel :: PrimitiveType -> Projector
projLabel = ProjPrim generated

projFun :: Projector
projFun = ProjFun generated

primInt :: PrimitiveType
primInt = PrimInt generated

primChar :: PrimitiveType
primChar = PrimChar generated

-- Derive appropriate Eq and Ord instances for these data types

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



