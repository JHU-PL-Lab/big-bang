{-# LANGUAGE MultiParamTypeClasses #-}
module Language.TinyBang.Types.Types
( Alpha(..)
, CallSite(..)
, CallSites
, AlphaId
, callSites
, unCallSites
, TauUp(..)
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
, Sigma(..)
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Function (on)

import Language.TinyBang.Types.UtilTypes (LabelName, Ident, LazyOperator, Sigma(..), PrimitiveType(..))
import Language.TinyBang.Render.Display
import qualified Language.TinyBang.Ast as A

-------------------------------------------------------------------------------
-- *Little Bang Types
-- $TinyBangTypes
--
-- These data types are used to represent Little Bang's type grammar.

-- |The thing we use to identify and distinguish alphas
type AlphaId = Integer

-- |The datatype used to represent type variables.
data Alpha = Alpha AlphaId CallSites
  deriving (Eq, Ord, Show)
  -- TODO: include data representing debugging hints etc. and rewrite Eq & Ord

-- |A data structure representing a single call site.
newtype CallSite = CallSite (Set Alpha)
  deriving (Eq, Ord, Show)

-- |A data structure representing function call sites.  The call site of a
--  function application is defined by the type parameter used as the domain
--  of the function type upper bounding the function in that application.  The
--  list of call sites is stored in reverse order; that is, the variable
--  '1^['2,'3] represents a1^{a3^{a2}}.  In the event of recursion, a set of
--  multiple call sites is grouped together.  For instance, the variable
--  '1^['3,'4,'3,'2] will be regrouped as the variable '1^[{'3,'4},'2].  Note
--  that, in this case, the use of single variables in the call site list is a
--  notational sugar for singleton sets.
newtype CallSites = CallSites { unCallSites :: [CallSite] }
  deriving (Eq, Ord, Show)
callSites :: [CallSite] -> CallSites
callSites lst = CallSites lst

-- |The datatype used to represent upper bound types.
data TauUp
  = TuFunc Alpha Alpha
  | TuCellGet Alpha
  | TuCellSet Alpha
  deriving (Eq, Ord, Show)

-- |The datatype used to represent lower bound types.
data TauDown
  = TdPrim PrimitiveType
  | TdLabel LabelName Alpha
  | TdOnion Alpha Alpha
  | TdLazyOp LazyOperator Alpha Alpha
  | TdFunc PolyFuncData
  | TdOnionSub Alpha Sigma
  | TdEmptyOnion
  | TdCell Alpha
  deriving (Eq, Ord, Show)


-- |A wrapper type containing the polymorphic function type information.
data PolyFuncData =
  PolyFuncData (Set Alpha) Alpha Alpha Constraints -- TODO: alias Set AnyAlpha?
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- *Type Pattern Types
-- $TypePatternTypes
--
-- These types are used to describe type patterns in Little Bang.

-- |A type representing the patterns produced by guards.
data TauChi
  = ChiPrim PrimitiveType
  | ChiLabel LabelName Alpha
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
  = LowerSubtype TauDown Alpha ConstraintHistory
  | UpperSubtype Alpha TauUp ConstraintHistory
  | AlphaSubtype Alpha Alpha ConstraintHistory
  | Case Alpha [Guard] ConstraintHistory
  | Bottom ConstraintHistory
  deriving (Show)

-- |A datatype used to simplify writing Ord and Eq instances for Constraint.
data ConstraintOrdinal
  = OrdLS TauDown Alpha
  | OrdUS Alpha TauUp
  | OrdAS Alpha Alpha
  | OrdCase Alpha [Guard]
  | OrdBottom ConstraintHistory
  deriving (Eq, Ord)

-- |Extracts information relevant for sorting
constraintOrdinal :: Constraint -> ConstraintOrdinal
constraintOrdinal c =
  case c of
    LowerSubtype td a  _ -> OrdLS     td a
    UpperSubtype a  tu _ -> OrdUS     a  tu
    AlphaSubtype a1 a2 _ -> OrdAS     a1 a2
    Case         a  gs _ -> OrdCase   a gs
    Bottom             h -> OrdBottom h

instance Eq Constraint where
  (==) = (==) `on` constraintOrdinal

instance Ord Constraint where
  compare = compare `on` constraintOrdinal

-- |A type describing the which rule generated a constraint and why.
data ConstraintHistory
  -- | Takes an AST nod and the environment local to that node
  = Inferred
      A.Expr
      (Map Ident Alpha)
  | IDontCare
  -- | The first argument is a td <: alpha.
  --   The second argument is an alpha <: tu.
  | ClosureTransitivity
      Constraint
      Constraint
  -- | The first argument is a td <: alpha.
  --   The second argument is a label alpha <: tu.
  | ClosureLabel
      Constraint
      Constraint
  -- | The first argument is a td <: alpha1.
  --   The second argument is a td <: alpha2.
  --   The third argument is an alpha1 & alpha2 <: tu.
  | ClosureOnion
      Constraint
      Constraint
      Constraint
  -- | The first argument is a td <: alpha.
  --   The second argument is a case constraint.
  | ClosureCase
      Constraint
      Constraint
  -- | The first argument is a td <: alpha.
  --   The second argument is a forall-quantified function <: alpha -> alpha.
  | ClosureApplication
      Constraint
      Constraint
  -- | The first argument is a td <: alpha.
  --   The second argument is a case constraint.
  | ContradictionCase
      Constraint
      Constraint
  -- | The argument is a prim1 <: prim2 where prim1 /= prim2.
  | ContradictionPrimMismatch
      Constraint
  -- | The argument is a lbl tdo <: prim.
  | ContradictionLabelPrim
      Constraint
  -- | The argument is a prim <: alpha -> alpha.
  | ContradictionPrimFunc
      Constraint
  -- | The argument is a forall-quantified function <: prim.
  | ContradictionFuncPrim
      Constraint
  -- | The argument is a lbl td <: alpha -> alpha.
  | ContradictionLabelFunc
      Constraint
  deriving (Eq, Ord, Show)

-- |A type representing guards in Little Bang case constraints.
data Guard = Guard TauChi Constraints
    deriving (Eq, Ord, Show)

class MkConstraint a b where
  (<:) :: a -> b -> ConstraintHistory -> Constraint

instance MkConstraint TauDown Alpha where
  (<:) = LowerSubtype

instance MkConstraint Alpha TauUp where
  (<:) = UpperSubtype

instance MkConstraint Alpha Alpha where
  (<:) = AlphaSubtype

instance MkConstraint PrimitiveType Alpha where
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

instance Display Alpha where
  makeDoc (Alpha i cSites) =
      char '\'' <> makeDoc i <> (
          let doc = makeDoc cSites in
          if not $ isEmpty doc
              then char '^' <> doc
              else empty)

instance Display CallSites where
  makeDoc sites =
      let siteList = reverse $ unCallSites sites in
      if length siteList == 0
          then empty
          else brackets $ hcat $ punctuate (text ", ") $ map sDoc siteList
    where sDoc (CallSite set) =
              if Set.size set == 1
                  then makeDoc $ Set.findMin set
                  else makeDoc set

instance Display CallSite where
  makeDoc (CallSite set) = makeDoc set

instance Display TauUp where
  makeDoc tau =
    case tau of
      TuFunc au a -> makeDoc au <+> text "->" <+> makeDoc a
      TuCellGet a -> text "CellG" <> parens (makeDoc a)
      TuCellSet a -> text "CellS" <> parens (makeDoc a)

instance Display TauDown where
  makeDoc tau =
    case tau of
      TdPrim p -> makeDoc p
      TdLabel n a -> char '`' <> makeDoc n <+> makeDoc a
      TdOnion a1 a2 -> makeDoc a1 <+> char '&' <+> makeDoc a2
      TdFunc polyFuncData -> makeDoc polyFuncData
      TdLazyOp op a1 a2 -> makeDoc a1 <+> makeDoc op <+> makeDoc a2
      TdEmptyOnion -> text "(&)"
      TdOnionSub a s -> makeDoc a <+> char '&' <> makeDoc s
      TdCell a -> text "Cell" <> parens (makeDoc a)

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
      Case alpha guards _ ->
        text "case" <+> makeDoc alpha <+> text "of" <+> lbrace $+$
        (nest indentSize $ vcat $ punctuate semi $ map gDoc guards)
        $+$ rbrace
      Bottom ch -> text "_|_" -- $$ (text . show) ch
    where gDoc (Guard tauChi constraints) =
            makeDoc tauChi <+> text "->" <+> makeDoc constraints
          subtype a b = makeDoc a <+> text "<:" <+> makeDoc b
