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
, ScapeData(..)
, PrimitiveType(..)
, PatternType(..)
, PrimaryPatternType(..)
, Constraints
, Constraint(..)
, ConstraintHistory(..)
, Guard(..)
, (<:)
, (.:)
, ProjTerm(..)
, CellGet(..)
, CellSet(..)
, Cell(..)
, UpFun(..)
, ForallVars(..)
, LazyOp(..)
, InterAlphaChain (..)
, CellAlphaChain (..)
, histFIXME
, module Language.TinyBang.Types.Alphas
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import Data.List (intersperse)
import Data.Function (on)

import Language.TinyBang.Config as Cfg
import Language.TinyBang.Types.UtilTypes
  (LabelName, Ident, LazyOperator, ProjTerm(..), PrimitiveType(..))
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
  | TdScape ScapeData
  | TdOnionSub InterAlpha ProjTerm
  | TdOnionProj InterAlpha ProjTerm
  | TdEmptyOnion
  deriving (Eq, Ord, Show)

newtype ForallVars = ForallVars (Set AnyAlpha)
  deriving (Eq, Ord, Show)
-- |A wrapper type containing the scape type information.
data ScapeData =
  ScapeData ForallVars PatternType InterAlpha Constraints
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- *Type Pattern Types
-- $TypePatternTypes
--
-- These types are used to describe type patterns in Little Bang.

-- |Data type describing top level type pattern types in case expressions;
--  corresponds to tau-chi in the document.
data PatternType = Pattern InterAlpha PrimaryPatternType
  deriving (Eq, Ord, Show)

data PrimaryPatternType
  = PatPrim PrimitiveType
  | PatLabel LabelName CellAlpha PrimaryPatternType
  | PatOnion [PrimaryPatternType] -- An empty one of these matches anything.
  | PatFun
  deriving (Eq, Ord, Show)

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
  | Comparable InterAlpha InterAlpha ConstraintHistory
  | Final InterAlpha ConstraintHistory
  | Immutable InterAlpha ConstraintHistory
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
    Comparable       a1 a2    _ -> OrdCmp    a1 a2
    Final            a1       _ -> OrdFin    a1
    Immutable        a1       _ -> OrdImmutable a1
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
data Guard = Guard PatternType Constraints
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
      TdScape scapeData -> makeDoc scapeData
      TdEmptyOnion -> text "(&)"
      TdOnionSub a s -> makeDoc a <+> text "&-" <+> makeDoc s
      TdOnionProj a s -> makeDoc a <+> text "&." <+> makeDoc s

instance Display ScapeData where
  makeDoc (ScapeData (ForallVars alphas) alpha1 alpha2 constraints) =
    nest indentSize $
    (if Set.size alphas > 0
      then text "all" <+> (parens $ makeDoc alphas)
      else empty) <+>
    makeDoc alpha1 <+> text "->" <+> makeDoc alpha2 <+>
    char '\\' <$$> (parens $ makeDoc constraints)
-- TODO: alpha lists are often in sequence; use a smarter doc creation for
--       some kind of .. notation

instance Display PatternType where
  makeDoc pat =
    case pat of
      Pattern a pp -> makeDoc a <> char '~' <> makeDoc pp

instance Display PrimaryPatternType where
  makeDoc pat =
    case pat of
      PatPrim tprim -> makeDoc tprim
      PatLabel lbl a pp -> makeDoc lbl <+> makeDoc a <> char '~' <> makeDoc pp
      PatOnion pps ->
        if null pps
        then text "any"
        else parens $ sep $ intersperse (char '&') $ map makeDoc pps
      PatFun -> text "fun"


instance Display Constraint where
  makeDoc c =
    let (base,hist) =
          case c of
            LowerSubtype a b h -> (subtype a b, h)
            UpperSubtype a b d h -> (subtype a $ dispFun b d, h)
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
              (subtype (makeDoc a1 <+> makeDoc op <+> makeDoc a2) a3, h)
            Comparable a1 a2 h ->
              (text "cmp" <> parens (makeDoc a1 <> text "," <> makeDoc a2), h)
            Final a h -> (text "final" <> parens (makeDoc a), h)
            Immutable a h -> (text "immutable" <> parens (makeDoc a), h)
            Bottom h -> (text "_|_", h)
    in
    if Cfg.displayDebugging
        then base <$$> (indent indentSize $ makeDoc hist)
        else base
    where subtype :: (Display a, Display b) => a -> b -> Doc
          subtype a b = makeDoc a <+> text "<:" <+> makeDoc b
          dispFun :: (Display a, Display b) => a -> b -> Doc
          dispFun a b = makeDoc a <+> text "->" <+> makeDoc b

instance Display ConstraintHistory where
  makeDoc hist =
    case hist of
      Inferred e env -> text "from expr" <+> makeDoc e <+> text "with env"
                        <+> makeDoc env
      ClosureCase c cn -> text "by case because" <$$>
                          (nest indentSize $ makeDoc c <+> tAnd
                                         <$$> makeDoc cn)
      ClosureApplication cn1 cn2 -> text "by application because" <$$>
                                    (nest indentSize $ makeDoc cn1 <+> tAnd
                                                   <$$> makeDoc cn2)
      ClosureLop c cn1 cn2 -> text "by lazy operation because" <$$>
                              (nest indentSize $ makeDoc c <+> tAnd
                                             <$$> makeDoc cn1 <+> tAnd
                                             <$$> makeDoc cn2)
      ClosureCellForward cn1 cn2 -> text "by cell forward prop. because" <$$>
                                    (nest indentSize $ makeDoc cn1 <+> tAnd
                                                   <$$> makeDoc cn2)
      ClosureCellBackward cn1 cn2 -> text "by cell backward prop. because" <$$>
                                     (nest indentSize $ makeDoc cn1 <+> tAnd
                                                    <$$> makeDoc cn2)
      ContradictionApplication cn ->
        text "by application contradiction because" <$$>
        (nest indentSize $ makeDoc cn)
      ContradictionCase c cn -> text "by case contradiction because" <$$>
                                (nest indentSize $ makeDoc c <+> tAnd
                                               <$$> makeDoc cn)
      ContradictionLop c cn -> text "by lazy op. contradiction because" <$$>
                               (nest indentSize $ makeDoc c <+> tAnd
                                              <$$> makeDoc cn)
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
