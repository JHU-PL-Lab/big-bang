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
, qualFinal
, qualImmutable
, qualNone
, projPrim
, projLabel
, projFun
, primInt
, primChar
) where

import Control.Applicative ((<$>))
import Text.PrettyPrint.Leijen hiding ((<$>),list)

import Language.TinyBang.Display
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

qualFinal :: CellQualifier
qualFinal = QualFinal generated

qualImmutable :: CellQualifier
qualImmutable = QualImmutable generated

qualNone :: CellQualifier
qualNone = QualNone generated

projPrim :: PrimitiveType -> Projector
projPrim = ProjPrim generated

projLabel :: LabelName -> Projector
projLabel = ProjLabel generated

projFun :: Projector
projFun = ProjFun generated

primInt :: PrimitiveType
primInt = PrimInt generated

primChar :: PrimitiveType
primChar = PrimChar generated

-- Display instances for the above types
-- TODO: include positional information somehow (via annotations?)

instance Display Expr where
  makeDoc (Expr _ cls) = sepDoc (text "; ") $ map makeDoc cls

instance Display Clause where
  makeDoc cl = case cl of
    RedexDef _ x r -> makeDoc x <+> text "=" <+> makeDoc r
    CellSet _ y x -> makeDoc y <+> text "<-" <+> makeDoc x
    CellGet _ x y -> makeDoc x <+> text "=" <+> text "!" <> makeDoc y
    Throws _ x x' -> makeDoc x <+> text "throws" <+> makeDoc x'
    Evaluated ecl -> makeDoc ecl
    
instance Display EvaluatedClause where
  makeDoc ecl = case ecl of
    ValueDef _ x v -> makeDoc x <+> text "=" <+> makeDoc v
    CellDef _ q y x -> makeDoc q <+> makeDoc y <+> text ":=" <+> makeDoc x
    Flow _ x k x' -> makeDoc x <+> text "<~" <> makeDoc k <+> makeDoc x'

instance Display Redex where
  makeDoc r = case r of
    Define _ x -> makeDoc x
    Appl _ x x' -> makeDoc x <+> makeDoc x'
    BinOp _ x op x' -> makeDoc x <+> makeDoc op <+> makeDoc x'
    
instance Display Value where
  makeDoc v = case v of
    VInt _ n -> text $ show n
    VChar _ c -> text "'" <> text [c] <> text "'"
    VEmptyOnion _ -> text "()"
    VLabel _ n x -> makeDoc n <+> makeDoc x
    VOnion _ x x' -> makeDoc x <+> text "&" <+> makeDoc x'
    VOnionFilter _ x op proj -> makeDoc x <+> makeDoc op <+> makeDoc proj
    VScape _ pat e -> makeDoc pat <+> text "->" <+> text "{" <+> makeDoc e
                          <+> text "}"

instance Display Pattern where
  makeDoc pat = case pat of
    ValuePattern _ y ipat -> makeDoc y <+> text ":" <+> makeDoc ipat
    ExnPattern _ y ipat -> text "exn" <+> makeDoc y <+> text ":" <+> makeDoc ipat

instance Display InnerPattern where
  makeDoc ipat = case ipat of
    PrimitivePattern _ p -> makeDoc p
    LabelPattern _ n y ipat' -> makeDoc n <+> makeDoc y <+> makeDoc ipat'
    ConjunctionPattern _ ipat' ipat'' -> text "(" <> makeDoc ipat' <> text ")"
      <+> text "&" <+> text "(" <> makeDoc ipat'' <> text ")"
    ScapePattern _ -> text "fun"
    EmptyOnionPattern _ -> text "()"

instance Display OnionOp where
  makeDoc op = case op of
    OpOnionSub _ -> text "&-"
    OpOnionProj _ -> text "&."

instance Display BinaryOperator where
  makeDoc op = case op of
    OpPlus _ -> text "+"
    OpMinus _ -> text "-"
    OpEqual _ -> text "="
    OpLess _ -> text "<"
    OpGreater _ -> text ">"

instance Display FlowKind where
  makeDoc k = case k of
    FlowExn -> text "x"
    
instance Display CellQualifier where
  makeDoc q = case q of
    QualFinal _ -> text "final"
    QualImmutable _ -> text "immut"
    QualNone _ -> empty
    
instance Display Projector where
  makeDoc proj = case proj of
    ProjPrim _ p -> makeDoc p
    ProjLabel _ n -> makeDoc n
    ProjFun _ -> text "fun"
    
instance Display PrimitiveType where
  makeDoc p = case p of
    PrimInt _ -> text "int"
    PrimChar _ -> text "char"

instance Display LabelName where
  makeDoc n = text "`" <> text (unLabelName n)
  
instance Display FlowVar where
  makeDoc x = text $ unFlowVar x
  
instance Display CellVar where
  makeDoc y = text $ unCellVar y
  
flattenOrigins :: Origin -> [SourceRegion]
flattenOrigins orig = case orig of
  SourceOrigin sr -> [sr]
  ComputedOrigin origs -> concatMap flattenOrigins origs

instance Display Origin where
  makeDoc orig = case orig of
    SourceOrigin sr -> makeDoc sr
    ComputedOrigin origs -> text "(" <> text "computed from" <+>
      makeDoc (concatMap flattenOrigins origs) <> text ")"

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



