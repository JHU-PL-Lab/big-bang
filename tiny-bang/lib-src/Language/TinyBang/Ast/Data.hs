{-# LANGUAGE TemplateHaskell, ViewPatterns, DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}

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
, AnyProjector(..)
, Projector(..)
, ProjectorTag(..)
, PrimitiveType(..)
, LabelName(..)
, FlowVar(..)
, CellVar(..)
, AnyVar(..)
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
, projInt
, projChar
, anyProjPrim
, anyProjLabel
, anyProjFun
, anyProjInt
, anyProjChar
, primInt
, primChar
, HasOrigin(..)
, IsVar(..)
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
  | VOnionFilter Origin FlowVar OnionOp AnyProjector
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
  | OpMult Origin
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

-- |A constructor-tagging ADT for projectors.  This type is promoted to a data
--  kind to allow different projector types to be tagged differently.
data ProjectorTag
  = ProjPrimTag
  | ProjLabelTag
  | ProjFunTag

-- |A data type for projectors.  This type is type-tagged to permit projection
--  to return a different type depending upon which kind of projector is used.
data Projector (tag :: ProjectorTag) where
  ProjPrim :: Origin -> PrimitiveType -> Projector ProjPrimTag
  ProjLabel :: Origin -> LabelName -> Projector ProjLabelTag
  ProjFun :: Origin -> Projector ProjFunTag
deriving instance Show (Projector tag)
  
-- |A data type containing a projector with an unknown tag.
data AnyProjector where
  SomeProjector :: Projector (tag :: ProjectorTag) -> AnyProjector
deriving instance Show AnyProjector

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

-- AnyVar appears below the TemplateHaskell which creates the Eq and Ord
-- instances below.
  
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

projPrim :: PrimitiveType -> Projector ProjPrimTag
projPrim = ProjPrim generated

projLabel :: LabelName -> Projector ProjLabelTag
projLabel = ProjLabel generated

projFun :: Projector ProjFunTag
projFun = ProjFun generated

projInt :: Projector ProjPrimTag
projInt = projPrim primInt

projChar :: Projector ProjPrimTag
projChar = projPrim primChar

anyProjPrim :: PrimitiveType -> AnyProjector
anyProjPrim = SomeProjector . projPrim

anyProjLabel :: LabelName -> AnyProjector
anyProjLabel = SomeProjector . projLabel

anyProjFun :: AnyProjector
anyProjFun = SomeProjector projFun

anyProjInt :: AnyProjector
anyProjInt = anyProjPrim primInt

anyProjChar :: AnyProjector
anyProjChar = anyProjPrim primChar

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
    CellDef _ q y x -> makeDoc q <> makeDoc y <+> text ":=" <+> makeDoc x
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
    LabelPattern _ n y ipat' ->
      makeDoc n <+> makeDoc y <+> text ":" <+> makeDoc ipat'
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
    OpMult _ -> text "*"
    OpEqual _ -> text "=="
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
    
instance Display AnyProjector where
  makeDoc (SomeProjector proj') = makeDoc proj'
    
instance Display (Projector tag) where
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
      
-- Manual Eq and Ord instances where necessary
instance Eq AnyProjector where
  a == b = (compare a b == EQ)
instance Ord AnyProjector where
  compare a b =
    case (a,b) of
      (SomeProjector (ProjPrim _ p), SomeProjector (ProjPrim _ p')) ->
        compare p p'
      (SomeProjector (ProjPrim _ _), SomeProjector (ProjLabel _ _)) -> LT
      (SomeProjector (ProjPrim _ _), SomeProjector (ProjFun _)) -> LT
      (SomeProjector (ProjLabel _ _), SomeProjector (ProjPrim _ _)) -> GT 
      (SomeProjector (ProjLabel _ l), SomeProjector (ProjLabel _ l')) ->
        compare l l'
      (SomeProjector (ProjLabel _ _), SomeProjector (ProjFun _)) -> LT
      (SomeProjector (ProjFun _), SomeProjector (ProjPrim _ _)) -> GT
      (SomeProjector (ProjFun _), SomeProjector (ProjLabel _ _)) -> GT
      (SomeProjector (ProjFun _), SomeProjector (ProjFun _)) -> EQ

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
      , ''PrimitiveType
      , ''LabelName
      , ''FlowVar
      , ''CellVar
      ]
  ])

-- |A wrapper for all variables.
data AnyVar
  = SomeFlowVar FlowVar
  | SomeCellVar CellVar
  deriving (Eq, Ord, Show)

instance Display AnyVar where
  makeDoc v = case v of
    SomeFlowVar x -> makeDoc x
    SomeCellVar y -> makeDoc y

-- A series of Regioned declarations for the AST types.
-- TODO: metaprogram these

-- |A typeclass for constructs containing a definitive source region.
class HasOrigin a where
  originOf :: a -> Origin

instance HasOrigin Origin where
  originOf = id

instance HasOrigin Expr where
  originOf x = case x of
    Expr orig _ -> orig

instance HasOrigin Clause where
  originOf x = case x of
    RedexDef orig _ _ -> orig
    CellSet orig _ _ -> orig
    CellGet orig _ _ -> orig
    Throws orig _ _ -> orig
    Evaluated c -> originOf c

instance HasOrigin EvaluatedClause where
  originOf x = case x of
    ValueDef orig _ _ -> orig
    CellDef orig _ _ _ -> orig
    Flow orig _ _ _ -> orig

instance HasOrigin Redex where
  originOf x = case x of
    Define orig _ -> orig
    Appl orig _ _ -> orig
    BinOp orig _ _ _ -> orig

instance HasOrigin Value where
  originOf x = case x of
    VInt orig _ -> orig
    VChar orig _ -> orig
    VEmptyOnion orig -> orig
    VLabel orig _ _ -> orig
    VOnion orig _ _ -> orig
    VOnionFilter orig _ _ _ -> orig
    VScape orig _ _ -> orig

instance HasOrigin Pattern where
  originOf x = case x of
    ValuePattern orig _ _ -> orig
    ExnPattern orig _ _ -> orig

instance HasOrigin InnerPattern where
  originOf x = case x of
    PrimitivePattern orig _ -> orig
    LabelPattern orig _ _ _ -> orig
    ConjunctionPattern orig _ _ -> orig
    ScapePattern orig -> orig
    EmptyOnionPattern orig -> orig

instance HasOrigin OnionOp where
  originOf x = case x of
    OpOnionSub orig -> orig
    OpOnionProj orig -> orig

instance HasOrigin BinaryOperator where
  originOf x = case x of
    OpPlus orig -> orig
    OpMinus orig -> orig
    OpMult orig -> orig
    OpEqual orig -> orig
    OpLess orig -> orig
    OpGreater orig -> orig

instance HasOrigin CellQualifier where
  originOf x = case x of
    QualFinal orig -> orig
    QualImmutable orig -> orig
    QualNone orig -> orig
    
instance HasOrigin AnyProjector where
  originOf (SomeProjector proj) = originOf proj

instance HasOrigin (Projector tag) where
  originOf x = case x of
    ProjPrim orig _ -> orig
    ProjLabel orig _ -> orig
    ProjFun orig -> orig

instance HasOrigin PrimitiveType where
  originOf x = case x of
    PrimInt orig -> orig
    PrimChar orig -> orig

instance HasOrigin LabelName where
  originOf x = case x of
    LabelName orig _ -> orig

instance HasOrigin FlowVar where
  originOf x = case x of
    FlowVar orig _ -> orig
    GenFlowVar orig _ _ -> orig

instance HasOrigin CellVar where
  originOf x = case x of
    CellVar orig _ -> orig
    GenCellVar orig _ _ -> orig

class IsVar a where
  someVar :: a -> AnyVar

instance IsVar AnyVar where
  someVar = id

instance IsVar FlowVar where
  someVar = SomeFlowVar

instance IsVar CellVar where
  someVar = SomeCellVar
