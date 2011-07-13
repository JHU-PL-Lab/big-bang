{-# LANGUAGE GADTs #-}

module Types 
(
) where

import Data.Set (Set)
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- *Big Bang Types
-- $BigBangTypes
--
-- These data types are used to represent Big Bang's type grammar.

-- |The datatype used to represent upper bound type variables.
data UpAlpha = UpAlpha Integer
    deriving (Eq, Ord)

-- |The datatype used to represent intermediate type variables.
data Alpha = Alpha Integer
    deriving (Eq, Ord)

-- |A wrapper datatype used to represent type variables.
data TVar = TAlpha Alpha | TUpAlpha UpAlpha
    deriving (Eq, Ord)

-- |The datatype used to represent upper bound types.
data TauUpOpen =
      TouPrim PrimitiveType
    | TouFunc UpAlpha Alpha
    | TouUpAlpha UpAlpha
    deriving (Eq, Ord)

-- |The datatype used to represent types which are either upper bound or
--  intermediate.
data TauUpClosed =
      TcuPrim PrimitiveType
    | TcuFunc UpAlpha Alpha
    | TcuUpAlpha UpAlpha
    | TcuAlpha Alpha
    deriving (Eq, Ord)

-- |The datatype used to represent lower bound types.
data TauDownOpen =
      TodPrim PrimitiveType
    | TodLabel String TauDownOpen
    | TodOnion TauDownOpen TauDownOpen
    | TodFunc (Set TVar) UpAlpha Alpha Constraints -- TODO: alias Set TVar?
    deriving (Eq, Ord)

-- |The datatype used to represent types which are either lower bound or
--  intermediate.
data TauDownClosed =
      TcdPrim PrimitiveType
    | TcdLabel String TauDownClosed
    | TcdOnion TauDownClosed TauDownClosed
    | TcdFunc (Set TVar) UpAlpha Alpha Constraints -- TODO: alias Set TVar?
    | TcdAlpha Alpha
    deriving (Eq, Ord)

-- |The datatype enumerating the primitives in the Big Bang type system.
data PrimitiveType =
      PrimInt
    | PrimString
    | PrimUnit
    deriving (Eq, Ord)


-------------------------------------------------------------------------------
-- *Auxiliary Types
-- $AuxiliaryTypes
--
-- These types are used in the definition of Big Bang types.

-- |A distinguished type for labels.
newtype Label = Label { unLabel :: String }
    deriving (Eq, Ord)
{- TODO: smarter constructor -}
label s = Label s

-------------------------------------------------------------------------------
-- *Type Pattern Types
-- $TypePatternTypes
--
-- These types are used to describe type patterns in Big Bang.

-- |A type representing the patterns produced by guards.
data TauChi =
      ChiPrim PrimitiveType
    | ChiLabel Label Alpha
    | ChiOnion Alpha Alpha
    | ChiFun Alpha
    deriving (Eq, Ord)

------------------------------------------------------------------------------
-- *Constraints
-- $Constraints
-- Declaring types to represent Big Bang constraints.

-- |A type alias defining the form of constraints.
type Constraints = Set Constraint
-- |A type describing constraints in Big Bang.
data Constraint =
      Subtype TauDownClosed TauUpClosed
    | Case UpAlpha [Guard]
    | Bottom
    deriving (Eq, Ord)
-- |A type representing guards in Big Bang case constraints.
data Guard = Guard TauChi Constraints
    deriving (Eq, Ord)
