{-# LANGUAGE MultiParamTypeClasses,
             GeneralizedNewtypeDeriving,
             FlexibleInstances,
             TypeSynonymInstances,
             ImplicitParams,
             GADTs,
             StandaloneDeriving
             #-}
module Language.TinyBang.Types.Types
( TauDown(..)
, TauProj(..)
, PolyFuncData(..)
, PrimitiveType(..)
, TauChi(..)
, Constraints
, Constraint(..)
, ConstraintHistory(..)
, Guard(..)
, (<:)
, (.:)
, SubTerm(..)
, CellGet(..)
, CellSet(..)
, Cell(..)
, UpFun(..)
, ForallVars(..)
, LazyOp(..)
, InterAlphaChain (..)
, CellAlphaChain (..)
, ConstraintOrdinal (..)
, constraintOrdinal
, histFIXME
, module Language.TinyBang.Types.Alphas
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Function (on)

import Language.TinyBang.Types.UtilTypes
  (LabelName, Ident, LazyOperator, SubTerm(..), PrimitiveType(..))
import Language.TinyBang.Types.Alphas
import qualified Language.TinyBang.Ast as A
import Utils.Render.Display

-------------------------------------------------------------------------------
-- *Little Bang Types
-- $TinyBangTypes
--
-- These data types are used to represent Little Bang's type grammar.

newtype CellGet = CellGet InterAlpha
  deriving (Eq, Ord, Show)
newtype CellSet = CellSet InterAlpha
  deriving (Eq, Ord, Show)
newtype Cell    = Cell    InterAlpha
  deriving (Eq, Ord, Show)
data    LazyOp  = LazyOp  LazyOperator InterAlpha InterAlpha
data    UpFun   = UpFun   CellAlpha    InterAlpha

-- -- |The datatype used to represent upper bound types.
-- data TauUp = TuFunc CellAlpha InterAlpha
--   deriving (Eq, Ord, Show)

-- |The datatype used to represent lower bound types.
data TauDown
  = TdPrim PrimitiveType
  | TdLabel LabelName CellAlpha
  | TdOnion InterAlpha InterAlpha
  | TdFunc PolyFuncData
  | TdOnionSub InterAlpha SubTerm
  | TdEmptyOnion
  deriving (Eq, Ord, Show)

-- |The datatype used to represent projection types.
data TauProj
  = TpPrim PrimitiveType
  | TpLabel LabelName
  | TpFun
  deriving (Eq, Ord, Show)

newtype ForallVars = ForallVars (Set AnyAlpha)
  deriving (Eq, Ord, Show)
-- |A wrapper type containing the polymorphic function type information.
data PolyFuncData =
  PolyFuncData ForallVars CellAlpha InterAlpha Constraints
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- *Type Pattern Types
-- $TypePatternTypes
--
-- These types are used to describe type patterns in Little Bang.

type TauChiMain = TauChi A.ChiMainType
type TauChiStruct = TauChi A.ChiStructType
type TauChiBind = TauChi A.ChiBindType
type TauChiPrimary = TauChi A.ChiPrimaryType

-- |Data type describing top level type pattern types in case expressions;
--  corresponds to tau-chi in the document.
data TauChi a where
  TauChiTopVar          :: InterAlpha                    -> TauChiMain
  TauChiTopOnion        :: TauChiPrimary -> TauChiStruct -> TauChiMain
  TauChiTopBind         :: TauChiBind                    -> TauChiMain

  TauChiOnionMany       :: TauChiPrimary -> TauChiStruct -> TauChiStruct
  TauChiOnionOne        :: TauChiPrimary                 -> TauChiStruct

  TauChiBound           :: InterAlpha -> TauChiBind -> TauChiBind
  TauChiUnbound         :: TauChiPrimary            -> TauChiBind

  TauChiPrim            :: PrimitiveType                  -> TauChiPrimary
  TauChiLabelShallow    :: LabelName       -> CellAlpha   -> TauChiPrimary
  TauChiLabelDeep       :: LabelName       -> TauChiBind  -> TauChiPrimary
  TauChiFun             ::                                   TauChiPrimary
  TauChiInnerStruct     :: TauChiStruct                   -> TauChiPrimary

deriving instance Show (TauChi a)
deriving instance Eq (TauChi a)
deriving instance Ord (TauChi a)

------------------------------------------------------------------------------
-- *Constraints
-- $Constraints
--
-- Declaring types to represent Little Bang constraints.

-- |A type alias defining the form of constraints.
type Constraints = Set Constraint

-- |A type describing constraints in Little Bang.
data Constraint
  = LowerSubtype TauDown InterAlpha ConstraintHistory
  | UpperSubtype InterAlpha CellAlpha InterAlpha ConstraintHistory
  | AlphaSubtype InterAlpha InterAlpha ConstraintHistory
  | CellSubtype InterAlpha CellAlpha ConstraintHistory
  | CellGetSubtype CellAlpha InterAlpha ConstraintHistory
  | CellSetSubtype CellAlpha InterAlpha ConstraintHistory
  | CellAlphaSubtype CellAlpha CellAlpha ConstraintHistory
  | LazyOpSubtype
      LazyOperator InterAlpha InterAlpha InterAlpha ConstraintHistory
  | Equivalent ProgramLabel ProgramLabel ConstraintHistory
  | Comparable InterAlpha InterAlpha ConstraintHistory
  | Final InterAlpha ConstraintHistory
  | Immutable InterAlpha ConstraintHistory
  | Case InterAlpha [Guard] ConstraintHistory
  | Bottom ConstraintHistory
  deriving (Show)

-- |A datatype used to simplify writing Ord and Eq instances for Constraint.
data ConstraintOrdinal
  = OrdLS TauDown InterAlpha
  | OrdUS InterAlpha CellAlpha InterAlpha
  | OrdAS InterAlpha InterAlpha
  | OrdCLS InterAlpha CellAlpha
  | OrdCGS CellAlpha InterAlpha
  | OrdCSS CellAlpha InterAlpha
  | OrdCAS CellAlpha CellAlpha
  | OrdLOS LazyOperator InterAlpha InterAlpha InterAlpha
  | OrdEqv ProgramLabel ProgramLabel
  | OrdCmp InterAlpha InterAlpha
  | OrdFin InterAlpha
  | OrdImmutable InterAlpha
  | OrdCase InterAlpha [Guard]
  | OrdBottom ConstraintHistory
  deriving (Eq, Ord)

-- |Extracts information relevant for sorting
constraintOrdinal :: Constraint -> ConstraintOrdinal
constraintOrdinal c =
  case c of
    LowerSubtype     td a     _ -> OrdLS     td a
    UpperSubtype     a  ca ia _ -> OrdUS     a  ca ia
    AlphaSubtype     a1 a2    _ -> OrdAS     a1 a2
    CellSubtype      td a     _ -> OrdCLS    td a
    CellGetSubtype   a  tu    _ -> OrdCGS    a  tu
    CellSetSubtype   a  tu    _ -> OrdCSS    a  tu
    CellAlphaSubtype a1 a2    _ -> OrdCAS    a1 a2
    LazyOpSubtype op a1 a2 a3 _ -> OrdLOS op a1 a2 a3
    Equivalent       p1 p2    _ -> OrdEqv    p1 p2
    Comparable       a1 a2    _ -> OrdCmp    a1 a2
    Final            a1       _ -> OrdFin    a1
    Immutable        a1       _ -> OrdImmutable a1
    Case             a  gs    _ -> OrdCase   a gs
    Bottom                    h -> OrdBottom h

instance Eq Constraint where
  (==) = (==) `on` constraintOrdinal

instance Ord Constraint where
  compare = compare `on` constraintOrdinal

-- TODO: These data structures allow malformed values, but they're
-- being used only for debugging; possibly change them later

data InterAlphaChain
  = IATerm TauDown
  | IALink InterAlpha Constraint InterAlphaChain
  | IAHead CellAlpha InterAlpha Constraint InterAlphaChain
  deriving (Eq, Ord, Show)

data CellAlphaChain
  = CATerm Cell
  | CALink CellAlpha Constraint CellAlphaChain
  | CAHeadG CellGet Constraint CellAlphaChain
  | CAHeadS CellSet Constraint CellAlphaChain
  deriving (Eq, Ord, Show)

-- |A type describing the which rule generated a constraint and why.
data ConstraintHistory
  -- | Takes an AST node and the environment local to that node
  = Inferred
      A.Expr
      (Map Ident CellAlpha)
  -- | The first argument is a case constraint.
  --   The second argument is a td <|: alpha.
  | ClosureCase
      Constraint
      InterAlphaChain
  -- | The first argument is a forall-quantified function <|: alpha -> alpha.
  --   The second argument is a Cell(alpha) <|: alpha.
  | ClosureApplication
      InterAlphaChain
      CellAlphaChain
  -- | The first argument is of the form a1 lop a2 <: a3.
  --   The second argument is int <|: a1
  --   The third argument is int <|: a2
  | ClosureLop
      Constraint
      InterAlphaChain
      InterAlphaChain
  -- | The first argument is Cell(a) <|: CellG(a')
  --   The second argument is t <|: a
  | ClosureCellForward
      CellAlphaChain
      InterAlphaChain
  -- | The first argument is Cell(a') <|: CellS(a)
  --   The second argument is t <|: a
  | ClosureCellBackward
      CellAlphaChain
      InterAlphaChain
  -- | The first argument is t <|: a -> a, where t is not a function
  | ContradictionApplication
      InterAlphaChain
  -- | The first argument is a case constraint.
  --   The second argument is a td <|: alpha.
  | ContradictionCase
      Constraint
      InterAlphaChain
  -- | The first argument is of the form a1 lop a2 <: a3.
  --   The second argument is t <|: a[12]
  --   Where t is not int
  | ContradictionLop
      Constraint
      InterAlphaChain
  -- TODO: eliminate this case
  | HistFIXME
  deriving (Eq, Ord, Show)

-- TODO: deprecate this
histFIXME :: ConstraintHistory
histFIXME = HistFIXME --error "History not implemented here!"

-- |A type representing guards in Little Bang case constraints.
data Guard = Guard TauChiMain Constraints
    deriving (Eq, Ord, Show)

class MkConstraint a b where
  (<:) :: a -> b -> ConstraintHistory -> Constraint

-- Not using the type synonyms to avoid weirdness with FlexibleInstances.
instance MkConstraint TauDown InterAlpha where
  (<:) = LowerSubtype

instance MkConstraint InterAlpha UpFun where
  a <: (UpFun ca ia) = UpperSubtype a ca ia

instance MkConstraint InterAlpha InterAlpha where
  (<:) = AlphaSubtype

instance MkConstraint Cell CellAlpha where
  (Cell ia) <: ca = CellSubtype ia ca

instance MkConstraint CellAlpha CellGet where
  ca <: (CellGet ia) = CellGetSubtype ca ia

instance MkConstraint CellAlpha CellSet where
  ca <: (CellSet ia) = CellSetSubtype ca ia

instance MkConstraint CellAlpha CellAlpha where
  (<:) = CellAlphaSubtype

instance MkConstraint LazyOp InterAlpha where
  (LazyOp op a1 a2) <: a3 = LazyOpSubtype op a1 a2 a3

instance MkConstraint PrimitiveType InterAlpha where
  p <: a = TdPrim p <: a

-- |An alias for application with precedence set so that @a <: b .: c@ creates
--  a subtype constraint with history @c@
(.:) :: (ConstraintHistory -> Constraint) -> ConstraintHistory -> Constraint
(.:) = ($)

infix 1 .:

-------------------------------------------------------------------------------
-- *Display Type Classes
-- $DisplayTypeClasses
--
-- Implementations of display routines for type structures.

instance Display TauDown where
  makeDoc tau =
    case tau of
      TdPrim p -> makeDoc p
      TdLabel n a -> char '`' <> makeDoc n <+> makeDoc a
      TdOnion a1 a2 -> makeDoc a1 <+> char '&' <+> makeDoc a2
      TdFunc polyFuncData -> makeDoc polyFuncData
      TdEmptyOnion -> text "(&)"
      TdOnionSub a s -> makeDoc a <+> char '&' <> makeDoc s

instance Display PolyFuncData where
  makeDoc (PolyFuncData (ForallVars alphas) alpha1 alpha2 constraints) =
    (if Set.size alphas > 0
      then text "all" <+> (parens $ makeDoc alphas)
      else empty) <+>
    makeDoc alpha1 <+> text "->" <+> makeDoc alpha2 <+>
    char '\\' <+> (parens $ makeDoc constraints)

instance Display TauChiMain where
  makeDoc tauChi =
    case tauChi of
      TauChiTopVar x -> makeDoc x
      TauChiTopOnion p s -> makeDoc p <+> text "&" <+> makeDoc s
      TauChiTopBind b -> makeDoc b

instance Display TauChiStruct where
  makeDoc tauChi =
    case tauChi of
      TauChiOnionMany p s -> makeDoc p <+> text "&" <+> makeDoc s
      TauChiOnionOne p -> makeDoc p

instance Display TauChiBind where
  makeDoc tauChi =
    case tauChi of
      TauChiBound i b -> makeDoc i <> text ":" <> makeDoc b
      TauChiUnbound p -> makeDoc p

instance Display TauChiPrimary where
  makeDoc tauChi =
    case tauChi of
      TauChiPrim p -> makeDoc p
      TauChiLabelShallow lbl x -> makeDoc lbl <+> makeDoc x
      TauChiLabelDeep lbl b -> makeDoc lbl <+> makeDoc b
      TauChiFun -> text "fun"
      TauChiInnerStruct s -> parens $ makeDoc s

instance Display Constraint where
  makeDoc c =
    let (base,hist) =
          case c of
            LowerSubtype a b h -> (subtype a b, h)
            UpperSubtype a b c h -> (subtype a $ dispFun b c, h)
            AlphaSubtype a b h -> (subtype a b, h)
            CellSubtype ia ca h ->
              (subtype (text "Cell" <> parens (makeDoc ia)) ca, h)
            CellGetSubtype ca ia h ->
              (subtype ca $ text "CellG" <> parens (makeDoc ia), h)
            CellSetSubtype ca ia h ->
              (subtype ca $ text "CellS" <> parens (makeDoc ia), h)
            CellAlphaSubtype a1 a2 h ->
              (subtype a1 a2, h)
            LazyOpSubtype op a1 a2 a3 h ->
              (subtype (makeDoc op <+> makeDoc a1 <+> makeDoc a2) a3, h)
            Equivalent p1 p2 h ->
              (hsep
               [makeDoc p1, text "-<", makeDoc p2]
              ,h)
            Comparable a1 a2 h ->
              (text "cmp" <> parens (makeDoc a1 <> text "," <> makeDoc a2), h)
            Case a gs h ->
              (text "case" <+> makeDoc a <+> text "of" <+> lbrace $+$
               (nest indentSize $ vcat $ punctuate semi $ map gDoc gs)
               $+$ rbrace
              ,h)
            Final a h -> (text "final" <> parens (makeDoc a), h)
            Immutable a h -> (text "immutable" <> parens (makeDoc a), h)
            Bottom h -> (text "_|_", h)
    in
    if ?debug then base $+$ (nest indentSize $ makeDoc hist)
              else base
    where gDoc (Guard tauChi constraints) =
            makeDoc tauChi <+> text "->" <+> makeDoc constraints
          subtype :: (Display a, Display b) => a -> b -> Doc
          subtype a b = makeDoc a <+> text "<:" <+> makeDoc b
          dispFun :: (Display a, Display b) => a -> b -> Doc
          dispFun a b = makeDoc a <+> text "->" <+> makeDoc b

instance Display ConstraintHistory where
  makeDoc hist =
    case hist of
      Inferred e env -> text "from expr" <+> makeDoc e <+> text "with env"
                        <+> makeDoc env
      ClosureCase c cn -> text "by case because" $+$
                          (nest indentSize $ makeDoc c <+> tAnd
                                         $+$ makeDoc cn)
      ClosureApplication cn1 cn2 -> text "by application because" $+$
                                    (nest indentSize $ makeDoc cn1 <+> tAnd
                                                   $+$ makeDoc cn2)
      ClosureLop c cn1 cn2 -> text "by lazy operation because" $+$
                              (nest indentSize $ makeDoc c <+> tAnd
                                             $+$ makeDoc cn1 <+> tAnd
                                             $+$ makeDoc cn2)
      ClosureCellForward cn1 cn2 -> text "by cell forward prop. because" $+$
                                    (nest indentSize $ makeDoc cn1 <+> tAnd
                                                   $+$ makeDoc cn2)
      ClosureCellBackward cn1 cn2 -> text "by cell backward prop. because" $+$
                                     (nest indentSize $ makeDoc cn1 <+> tAnd
                                                    $+$ makeDoc cn2)
      ContradictionApplication cn ->
        text "by application contradiction because" $+$
        (nest indentSize $ makeDoc cn)
      ContradictionCase c cn -> text "by case contradiction because" $+$
                                (nest indentSize $ makeDoc c <+> tAnd
                                               $+$ makeDoc cn)
      ContradictionLop c cn -> text "by lazy op. contradiction because" $+$
                               (nest indentSize $ makeDoc c <+> tAnd
                                              $+$ makeDoc cn)
      HistFIXME -> text "because of reasons!!"
    where tAnd = text "and"

-- TODO: print proof that subtypes in chain are valid
instance Display InterAlphaChain where
  makeDoc ch =
    case ch of
      IATerm td -> makeDoc td
      IALink ia _ ch' -> makeDoc ch' <+> text "<:" <+> makeDoc ia
      IAHead ca ia _ ch' ->
        makeDoc ch' <+> text "<:" <+> makeDoc ca <+> text "->" <+> makeDoc ia

-- TODO: print proof that subtypes in chain are valid
instance Display CellAlphaChain where
  makeDoc ch =
    case ch of
      CATerm (Cell ia) -> text "Cell(" <> makeDoc ia <> text ")"
      CALink ca _ ch' -> makeDoc ch' <+> text "<:" <+> makeDoc ca
      CAHeadG (CellGet ia) _ ch' ->
        makeDoc ch' <+> text "<:" <+> text "CellG(" <> makeDoc ia <> text ")"
      CAHeadS (CellSet ia) _ ch' ->
        makeDoc ch' <+> text "<:" <+> text "CellS(" <> makeDoc ia <> text ")"
