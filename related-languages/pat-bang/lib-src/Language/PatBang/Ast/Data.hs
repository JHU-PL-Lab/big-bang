{-# LANGUAGE TemplateHaskell, ViewPatterns, DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}

{-|
  A module which defines the data structures which comprise the PatBang ANF
  AST.
-}

module Language.PatBang.Ast.Data
( Expr(..)
, Clause(..)
, Redex(..)
, AstList(..)
, Value(..)
, PatternBody(..)
, BinaryOperator(..)
, AnyProjector(..)
, Projector(..)
, ProjectorTag(..)
, PrimitiveType(..)
, LabelName(..)
, FlowVar(..)
, PatVar(..)
, Origin(..)
, generated
, unLabelName
, unFlowVar
, unPatVar
, projPrim
, projLabel
, projFun
, projPat
, projScape
, projInt
, anyProjPrim
, anyProjLabel
, anyProjFun
, anyProjPat
, anyProjScape
, anyProjInt
, primInt
, HasOrigin(..)
) where

import Control.Applicative ((<$>))
import Text.PrettyPrint.Leijen hiding ((<$>),list)

import Language.PatBang.Display
import Language.PatBang.Syntax.Location
import Utils.Meta.Deriving

-- |A data type representing expressions.
data Expr
  = Expr Origin [Clause]
  deriving (Show)

-- |A data type representing general clauses.
data Clause
  = RedexDef Origin FlowVar Redex
  | ValueDef Origin FlowVar Value
  deriving (Show)

-- |A data type representing reducible expressions.
data Redex
  = Define Origin FlowVar
  | Appl Origin FlowVar FlowVar
  | BinOp Origin FlowVar BinaryOperator FlowVar
  deriving (Show)

-- |A data type representing a list of AST components.
data AstList a = AstList Origin [a]
  deriving (Eq, Ord, Show)

-- |A data type representing value forms.
data Value
  = VInt Origin Integer
  | VEmptyOnion Origin
  | VLabel Origin LabelName FlowVar
  | VOnion Origin FlowVar FlowVar
  | VFunction Origin (AstList FlowVar) Expr
  | VPattern Origin (AstList PatVar) PatternBody
  | VScape Origin FlowVar FlowVar
  deriving (Show)
  
-- |A data type representing pattern bodies.
data PatternBody
  = PPrim Origin PrimitiveType
  | PLabel Origin LabelName PatternBody
  | PFun Origin
  | PPat Origin
  | PScape Origin
  | PConj Origin PatternBody PatternBody
  | PSubst Origin FlowVar (AstList PatternBody)
  | PRec Origin PatVar PatternBody
  | PVar Origin PatVar
  deriving (Show)

-- |An enumeration of binary value operators.
data BinaryOperator
  = OpPlus Origin
  | OpMinus Origin
  | OpEqual Origin
  | OpGreater Origin
  | OpLess Origin
  deriving (Show)

-- |A constructor-tagging ADT for projectors.  This type is promoted to a data
--  kind to allow different projector types to be tagged differently.
data ProjectorTag
  = ProjPrimTag
  | ProjLabelTag
  | ProjFunTag
  | ProjPatTag
  | ProjScapeTag

-- |A data type for projectors.  This type is type-tagged to permit projection
--  to return a different type depending upon which kind of projector is used.
data Projector (tag :: ProjectorTag) where
  ProjPrim :: Origin -> PrimitiveType -> Projector ProjPrimTag
  ProjLabel :: Origin -> LabelName -> Projector ProjLabelTag
  ProjFun :: Origin -> Projector ProjFunTag
  ProjPat :: Origin -> Projector ProjPatTag
  ProjScape :: Origin -> Projector ProjScapeTag
deriving instance Show (Projector tag)
  
-- |A data type containing a projector with an unknown tag.
data AnyProjector where
  SomeProjector :: Projector (tag :: ProjectorTag) -> AnyProjector
deriving instance Show AnyProjector

-- |A representation of primitive types.
data PrimitiveType
  = PrimInt Origin
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
  
-- |A semantic wrapper for pattern variables.
data PatVar
  = PatVar Origin String
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
  
unPatVar :: PatVar -> String
unPatVar y = case y of
  PatVar _ s -> s
  
-- Smart constructors for the above data types

generated :: Origin
generated = ComputedOrigin []

projPrim :: PrimitiveType -> Projector ProjPrimTag
projPrim = ProjPrim generated

projLabel :: LabelName -> Projector ProjLabelTag
projLabel = ProjLabel generated

projFun :: Projector ProjFunTag
projFun = ProjFun generated

projPat :: Projector ProjPatTag
projPat = ProjPat generated

projScape :: Projector ProjScapeTag
projScape = ProjScape generated

projInt :: Projector ProjPrimTag
projInt = projPrim primInt

anyProjPrim :: PrimitiveType -> AnyProjector
anyProjPrim = SomeProjector . projPrim

anyProjLabel :: LabelName -> AnyProjector
anyProjLabel = SomeProjector . projLabel

anyProjFun :: AnyProjector
anyProjFun = SomeProjector projFun

anyProjPat :: AnyProjector
anyProjPat = SomeProjector projPat

anyProjScape :: AnyProjector
anyProjScape = SomeProjector projScape

anyProjInt :: AnyProjector
anyProjInt = anyProjPrim primInt

primInt :: PrimitiveType
primInt = PrimInt generated

-- Display instances for the above types
-- TODO: include positional information somehow (via annotations?)

instance Display Expr where
  makeDoc (Expr _ cls) = sepDoc (text "; ") $ map makeDoc cls

instance Display Clause where
  makeDoc cl = case cl of
    RedexDef _ x r -> makeDoc x <+> text "=" <+> makeDoc r
    ValueDef _ x v -> makeDoc x <+> text "=" <+> makeDoc v

instance Display Redex where
  makeDoc r = case r of
    Define _ x -> makeDoc x
    Appl _ x x' -> makeDoc x <+> makeDoc x'
    BinOp _ x op x' -> makeDoc x <+> makeDoc op <+> makeDoc x'
    
instance Display a => Display (AstList a) where
  makeDoc a = case a of
    AstList _ els -> text "(" <+> sepDoc (text ", ") (map makeDoc els)
                        <+> text ")"
    
instance Display Value where
  makeDoc v = case v of
    VInt _ n -> text $ show n
    VEmptyOnion _ -> text "()"
    VLabel _ n x -> makeDoc n <+> makeDoc x
    VOnion _ x x' -> makeDoc x <+> text "&" <+> makeDoc x'
    VFunction _ xs e -> text "(" <+> makeDoc xs <+> text ")" <+> text "->"
                          <+> text "{" <+> makeDoc e <+> text "}"
    VPattern _ ys p -> text "(" <+> makeDoc ys <+> text ")" <+> text "<-"
                          <+> text "{" <+> makeDoc p <+> text "}"
    VScape _ x x' -> makeDoc x <+> text "><" <+> makeDoc x'

instance Display PatternBody where
  makeDoc pat = case pat of
    PPrim _ t -> makeDoc t
    PLabel _ n p -> makeDoc n <+> makeDoc  p
    PFun _ -> text "fun"
    PPat _ -> text "pat"
    PScape _ -> text "scape"
    PConj _ p1 p2 -> makeDoc p1 <+> text "&" <+> makeDoc p2
    PSubst _ x ps -> makeDoc x <+> text "(" <+> makeDoc ps  <+> text ")"
    PRec _ y p -> text "rec" <+> makeDoc y <> text ":" <+> makeDoc p
    PVar _ y -> makeDoc y
-- TODO: correct to ensure proper parse priority

instance Display BinaryOperator where
  makeDoc op = case op of
    OpPlus _ -> text "+"
    OpMinus _ -> text "-"
    OpEqual _ -> text "="
    OpGreater _ -> text ">"
    OpLess _ -> text "<"

instance Display AnyProjector where
  makeDoc (SomeProjector proj') = makeDoc proj'
    
instance Display (Projector tag) where
  makeDoc proj = case proj of
    ProjPrim _ p -> makeDoc p
    ProjLabel _ n -> makeDoc n
    ProjFun _ -> text "fun"
    ProjPat _ -> text "pat"
    ProjScape _ -> text "scape"
    
instance Display PrimitiveType where
  makeDoc p = case p of
    PrimInt _ -> text "int"

instance Display LabelName where
  makeDoc n = text "`" <> text (unLabelName n)
  
instance Display FlowVar where
  makeDoc x = text $ unFlowVar x
  
instance Display PatVar where
  makeDoc y = text $ unPatVar y
  
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
      (SomeProjector (ProjPrim _ _), SomeProjector (ProjPat _)) -> LT
      (SomeProjector (ProjPrim _ _), SomeProjector (ProjScape _)) -> LT
      (SomeProjector (ProjLabel _ _), SomeProjector (ProjPrim _ _)) -> GT 
      (SomeProjector (ProjLabel _ l), SomeProjector (ProjLabel _ l')) ->
        compare l l'
      (SomeProjector (ProjLabel _ _), SomeProjector (ProjFun _)) -> LT
      (SomeProjector (ProjLabel _ _), SomeProjector (ProjPat _)) -> LT
      (SomeProjector (ProjLabel _ _), SomeProjector (ProjScape _)) -> LT
      (SomeProjector (ProjFun _), SomeProjector (ProjPrim _ _)) -> GT
      (SomeProjector (ProjFun _), SomeProjector (ProjLabel _ _)) -> GT
      (SomeProjector (ProjFun _), SomeProjector (ProjFun _)) -> EQ
      (SomeProjector (ProjFun _), SomeProjector (ProjPat _)) -> LT
      (SomeProjector (ProjFun _), SomeProjector (ProjScape _)) -> LT
      (SomeProjector (ProjPat _), SomeProjector (ProjPrim _ _)) -> GT
      (SomeProjector (ProjPat _), SomeProjector (ProjLabel _ _)) -> GT
      (SomeProjector (ProjPat _), SomeProjector (ProjFun _)) -> GT
      (SomeProjector (ProjPat _), SomeProjector (ProjPat _)) -> EQ
      (SomeProjector (ProjPat _), SomeProjector (ProjScape _)) -> LT
      (SomeProjector (ProjScape _), SomeProjector (ProjPrim _ _)) -> GT
      (SomeProjector (ProjScape _), SomeProjector (ProjLabel _ _)) -> GT
      (SomeProjector (ProjScape _), SomeProjector (ProjFun _)) -> GT
      (SomeProjector (ProjScape _), SomeProjector (ProjPat _)) -> GT
      (SomeProjector (ProjScape _), SomeProjector (ProjScape _)) -> EQ

-- Derive appropriate Eq and Ord instances for these data types

$(concat <$> sequence
  [ f name
  | f <- [deriveEqSkipFirst, deriveOrdSkipFirst]
  , name <-
      [ ''Expr
      , ''Clause
      , ''Redex
      , ''Value
      , ''PatternBody
      , ''BinaryOperator
      , ''PrimitiveType
      , ''LabelName
      , ''FlowVar
      , ''PatVar
      ]
  ])

-- A series of Regioned declarations for the AST types.
-- TODO: metaprogram these

-- |A typeclass for constructs containing a definitive source region.
class HasOrigin a where
  originOf :: a -> Origin

instance HasOrigin Expr where
  originOf x = case x of
    Expr orig _ -> orig

instance HasOrigin Clause where
  originOf x = case x of
    RedexDef orig _ _ -> orig
    ValueDef orig _ _ -> orig

instance HasOrigin Redex where
  originOf x = case x of
    Define orig _ -> orig
    Appl orig _ _ -> orig
    BinOp orig _ _ _ -> orig
    
instance HasOrigin (AstList a) where
  originOf x = case x of
    AstList orig _ -> orig

instance HasOrigin Value where
  originOf x = case x of
    VInt orig _ -> orig
    VEmptyOnion orig -> orig
    VLabel orig _ _ -> orig
    VOnion orig _ _ -> orig
    VFunction orig _ _ -> orig
    VPattern orig _ _ -> orig
    VScape orig _ _ -> orig

instance HasOrigin PatternBody where
  originOf x = case x of
    PPrim orig _ -> orig
    PLabel orig _ _ -> orig
    PFun orig -> orig
    PPat orig -> orig
    PScape orig -> orig
    PConj orig _ _ -> orig
    PSubst orig _ _ -> orig
    PRec orig _ _ -> orig
    PVar orig _ -> orig

instance HasOrigin BinaryOperator where
  originOf x = case x of
    OpPlus orig -> orig
    OpMinus orig -> orig
    OpEqual orig -> orig
    OpLess orig -> orig
    OpGreater orig -> orig

instance HasOrigin AnyProjector where
  originOf (SomeProjector proj) = originOf proj

instance HasOrigin (Projector tag) where
  originOf x = case x of
    ProjPrim orig _ -> orig
    ProjLabel orig _ -> orig
    ProjFun orig -> orig
    ProjPat orig -> orig
    ProjScape orig -> orig

instance HasOrigin PrimitiveType where
  originOf x = case x of
    PrimInt orig -> orig

instance HasOrigin LabelName where
  originOf x = case x of
    LabelName orig _ -> orig

instance HasOrigin FlowVar where
  originOf x = case x of
    FlowVar orig _ -> orig
    GenFlowVar orig _ _ -> orig

instance HasOrigin PatVar where
  originOf x = case x of
    PatVar orig _ -> orig
