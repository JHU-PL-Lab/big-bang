{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-|
  Defines the data types used in the naive type system implementation.
-}
module Language.TinyBang.TypeSystem.Simple.Data
( FilteredType(..)
, Type(..)
, TVar(..)
, ConstraintSet(..)
, Constraint(..)
, PatternTypeSet(..)
, PatternType(..)
, FilterType(..)

, TypecheckError(..)
, Inconsistency(..)

, (<:)
) where

import Data.Function
import Data.Map (Map)
import Data.Monoid hiding ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.Utils.Display

-- Data types ------------------------------------------------------------------

data Constraint
  = LowerBoundConstraint FilteredType TVar
  | IntermediateConstraint TVar TVar
  | ApplicationConstraint TVar TVar TVar
  | BuiltinOpConstraint BuiltinOp [TVar] TVar
  deriving (Eq, Ord, Show)

data FilteredType
  = FilteredType { filteredType :: Type
                 , positiveFilters :: PatternTypeSet
                 , negativeFilters :: PatternTypeSet
                 }
  deriving (Eq, Ord, Show)

data Type
  = TPrimitive PrimitiveType
  | TEmptyOnion
  | TLabel LabelName TVar
  | TRef TVar
  | TOnion TVar TVar
  | TScape PatternType TVar ConstraintSet
  deriving (Eq, Ord, Show)
  
data TVar
  = TVar Var PossibleContour
  deriving (Eq, Ord, Show)

newtype ConstraintSet
  = ConstraintSet { unConstraintSet :: Set Constraint }
  deriving (Eq, Ord, Show)
  
newtype PatternTypeSet
  = PatternTypeSet { unPatternTypeSet :: Set PatternType }
  deriving (Eq, Ord, Show)
  
data PatternType
  = PatternType TVar (Map TVar FilterType)
  deriving (Eq, Ord, Show)

data FilterType
  = TFPrim PrimitiveType
  | TFEmpty
  | TFLabel LabelName TVar
  | TFRef TVar
  | TFConjunction TVar TVar
  deriving (Eq, Ord, Show)
  
-- Closure results -------------------------------------------------------------

data TypecheckError
  = TypecheckInconsistent Inconsistency
  deriving (Eq, Ord, Show)

data Inconsistency
  = BuiltinBadOperandCount TVar BuiltinOp Int Int
      -- ^Indicates that a builtin operator was used with an incorrect number of
      --  operands.  The arguments are the use site, the operator used, the
      --  number of arguments expected, and the number of arguments found.
  | BuiltinBadOperandType TVar BuiltinOp Int TVar
      -- ^Indicates that a builtin operator was used with an argument of an
      --  incorrect type.  The arguments are the use site, the operator used,
      --  the index of the incorrect argument (from one), and the type variable
      --  of that argument.
  | TODO_ReplaceMe_Inconsistency -- TODO
  deriving (Eq, Ord, Show)

-- Convenient instances --------------------------------------------------------

instance Monoid ConstraintSet where
  mempty = ConstraintSet Set.empty
  mappend a b = ConstraintSet $ (Set.union `on` unConstraintSet) a b
  mconcat = ConstraintSet . Set.unions . map unConstraintSet

instance Monoid PatternTypeSet where
  mempty = PatternTypeSet Set.empty
  mappend a b = PatternTypeSet $ (Set.union `on` unPatternTypeSet) a b
  mconcat = PatternTypeSet . Set.unions . map unPatternTypeSet

-- Constructor sugar -----------------------------------------------------------

class SubtypeConstructable a b where
  (<:) :: a -> b -> Constraint
  infix 7 <:

instance SubtypeConstructable FilteredType TVar where
  (<:) = LowerBoundConstraint

instance SubtypeConstructable TVar TVar where
  (<:) = IntermediateConstraint
  
instance SubtypeConstructable (TVar,TVar) TVar where
  (<:) = uncurry ApplicationConstraint

-- Display instances -----------------------------------------------------------

instance Display Constraint where
  makeDoc c = case c of
    LowerBoundConstraint rt a ->
      parens (makeDoc rt) <+> text "<:" <+> makeDoc a
    IntermediateConstraint a1 a2 ->
      makeDoc a1 <+> text "<:" <+> makeDoc a2
    ApplicationConstraint a0 a1 a2 ->
      makeDoc a0 <+> makeDoc a1 <+> text "<:" <+> makeDoc a2
    BuiltinOpConstraint op as a ->
      makeDoc op <+> makeDoc as <+> text "<:" <+> makeDoc a

instance Display FilteredType where
  makeDoc (FilteredType t pp pn) =
    makeDoc t <+> char '|' <> char '+' <> makeDoc pp <> char ',' <> char '-' <>
      makeDoc pn

instance Display Type where
  makeDoc t = case t of
    TPrimitive pt -> makeDoc pt
    TEmptyOnion -> text "()"
    TLabel n a -> makeDoc n <+> makeDoc a
    TRef a -> text "ref" <+> makeDoc a
    TOnion a1 a2 -> makeDoc a1 <+> char '&' <+> makeDoc a2
    TScape pt a cs ->
      makeDoc pt <+> text "->" <+> makeDoc a <> char '\\' <> makeDoc cs

instance Display TVar where
  makeDoc (TVar x pcntr) =
    char 'α' <> makeDoc x <> char '^' <> makeDoc pcntr

instance Display ConstraintSet where
  makeDoc (ConstraintSet cs) = makeDoc cs

instance Display PatternTypeSet where
  makeDoc (PatternTypeSet ps) = makeDoc ps

instance Display PatternType where
  makeDoc (PatternType a pfm) = makeDoc a <> char '\\' <+> makeDoc pfm

instance Display FilterType where
  makeDoc filt = case filt of
    TFEmpty -> text "()"
    TFPrim tprim -> makeDoc tprim
    TFLabel n x -> makeDoc n <+> makeDoc x
    TFRef x -> text "ref" <+> makeDoc x
    TFConjunction x1 x2 -> makeDoc x1 <+> char '*' <+> makeDoc x2

instance Display TypecheckError where
  makeDoc err = case err of
    TypecheckInconsistent incon -> makeDoc incon

instance Display Inconsistency where
  makeDoc = const $ text "Inconsistent!" -- TODO
