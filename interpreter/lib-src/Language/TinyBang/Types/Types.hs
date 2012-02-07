{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Language.TinyBang.Types.Types
( TauUp(..)
, TauDown(..)
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
, ForallVars
, LazyOp(..)
, InterAlphaChain (..)
, CellAlphaChain (..)
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

-- |The datatype used to represent upper bound types.
data TauUp = TuFunc CellAlpha InterAlpha
  deriving (Eq, Ord, Show)

-- |The datatype used to represent lower bound types.
data TauDown
  = TdPrim PrimitiveType
  | TdLabel LabelName CellAlpha
  | TdOnion InterAlpha InterAlpha
  | TdFunc PolyFuncData
  | TdOnionSub InterAlpha SubTerm
  | TdEmptyOnion
  deriving (Eq, Ord, Show)

type ForallVars = Set AnyAlpha
-- |A wrapper type containing the polymorphic function type information.
data PolyFuncData =
  PolyFuncData ForallVars CellAlpha InterAlpha Constraints
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- *Type Pattern Types
-- $TypePatternTypes
--
-- These types are used to describe type patterns in Little Bang.

-- |A type representing the patterns produced by guards.
data TauChi
  = ChiPrim PrimitiveType
  | ChiLabel LabelName CellAlpha
  | ChiFun
  | ChiAny
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
  | UpperSubtype InterAlpha TauUp ConstraintHistory
  | AlphaSubtype InterAlpha InterAlpha ConstraintHistory
  | CellSubtype InterAlpha CellAlpha ConstraintHistory
  | CellGetSubtype CellAlpha InterAlpha ConstraintHistory
  | CellSetSubtype CellAlpha InterAlpha ConstraintHistory
  | CellAlphaSubtype CellAlpha CellAlpha ConstraintHistory
  | LazyOpSubtype
      LazyOperator InterAlpha InterAlpha InterAlpha ConstraintHistory
  | Comparable InterAlpha InterAlpha ConstraintHistory
  | Case InterAlpha [Guard] ConstraintHistory
  | Bottom ConstraintHistory
  deriving (Show)

-- |A datatype used to simplify writing Ord and Eq instances for Constraint.
data ConstraintOrdinal
  = OrdLS TauDown InterAlpha
  | OrdUS InterAlpha TauUp
  | OrdAS InterAlpha InterAlpha
  | OrdCLS InterAlpha CellAlpha
  | OrdCGS CellAlpha InterAlpha
  | OrdCSS CellAlpha InterAlpha
  | OrdCAS CellAlpha CellAlpha
  | OrdLOS LazyOperator InterAlpha InterAlpha InterAlpha
  | OrdCmp InterAlpha InterAlpha
  | OrdCase InterAlpha [Guard]
  | OrdBottom ConstraintHistory
  deriving (Eq, Ord)

-- |Extracts information relevant for sorting
constraintOrdinal :: Constraint -> ConstraintOrdinal
constraintOrdinal c =
  case c of
    LowerSubtype     td a     _ -> OrdLS     td a
    UpperSubtype     a  tu    _ -> OrdUS     a  tu
    AlphaSubtype     a1 a2    _ -> OrdAS     a1 a2
    CellSubtype      td a     _ -> OrdCLS    td a
    CellGetSubtype   a  tu    _ -> OrdCGS    a  tu
    CellSetSubtype   a  tu    _ -> OrdCSS    a  tu
    CellAlphaSubtype a1 a2    _ -> OrdCAS    a1 a2
    LazyOpSubtype op a1 a2 a3 _ -> OrdLOS op a1 a2 a3
    Comparable       a1 a2    _ -> OrdCmp    a1 a2
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
  | IALink InterAlpha InterAlphaChain
  | IAHead TauUp InterAlphaChain
  deriving (Eq, Ord, Show)

data CellAlphaChain
  = CATerm Cell
  | CALink CellAlpha CellAlphaChain
  | CAHeadG CellGet CellAlphaChain
  | CAHeadS CellSet CellAlphaChain
  deriving (Eq, Ord, Show)

-- |A type describing the which rule generated a constraint and why.
data ConstraintHistory
  -- | Takes an AST nod and the environment local to that node
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
  | ContradictionAppliction
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
  deriving (Eq, Ord, Show)

-- |A type representing guards in Little Bang case constraints.
data Guard = Guard TauChi Constraints
    deriving (Eq, Ord, Show)

class MkConstraint a b where
  (<:) :: a -> b -> ConstraintHistory -> Constraint

-- Not using the type synonyms to avoid weirdness with FlexibleInstances.
instance MkConstraint TauDown InterAlpha where
  (<:) = LowerSubtype

instance MkConstraint InterAlpha TauUp where
  (<:) = UpperSubtype

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

-- -- |An infix function for creating subtype contraints (for convenience).
-- --  meant to be used in conjunction with '(.:)'
-- (<:) :: TauDown -> TauUp -> ConstraintHistory -> Constraint
-- (<:) = Subtype

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

instance Display TauUp where
  makeDoc tau =
    case tau of
      TuFunc au a -> makeDoc au <+> text "->" <+> makeDoc a

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
  makeDoc (PolyFuncData alphas alpha1 alpha2 constraints) =
    (if Set.size alphas > 0
      then text "all" <+> (parens $ makeDoc alphas)
      else empty) <+>
    makeDoc alpha1 <+> text "->" <+> makeDoc alpha2 <+>
    char '\\' <+> (parens $ makeDoc constraints)

instance Display TauChi where
  makeDoc tauChi =
    case tauChi of
      ChiPrim p -> makeDoc p
      ChiLabel n au -> char '`' <> makeDoc n <+> makeDoc au
      ChiFun -> text "fun"
      ChiAny -> text "_"

instance Display Constraint where
  makeDoc c =
    --TODO: FIXME display history or remove this comment
    case c of
      LowerSubtype a b _ -> subtype a b
      UpperSubtype a b _ -> subtype a b
      AlphaSubtype a b _ -> subtype a b
      CellSubtype ia ca _ ->
        subtype (text "Cell" <> parens (makeDoc ia)) ca
      CellGetSubtype ca ia _ ->
        subtype ca $ text "CellG" <> parens (makeDoc ia)
      CellSetSubtype ca ia _ ->
        subtype ca $ text "CellS" <> parens (makeDoc ia)
      CellAlphaSubtype a1 a2 _ ->
        subtype a1 a2
      LazyOpSubtype op a1 a2 a3 _ ->
        subtype (makeDoc op <+> makeDoc a1 <+> makeDoc a2) a3
      Comparable a1 a2 _ ->
        text "cmp" <> parens (makeDoc a1 <> text "," <> makeDoc a2)
      Case a gs _ ->
        text "case" <+> makeDoc a <+> text "of" <+> lbrace $+$
        (nest indentSize $ vcat $ punctuate semi $ map gDoc gs)
        $+$ rbrace
      Bottom _ -> text "_|_" -- $$ (text . show) ch
    where gDoc (Guard tauChi constraints) =
            makeDoc tauChi <+> text "->" <+> makeDoc constraints
          subtype a b = makeDoc a <+> text "<:" <+> makeDoc b
