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
    -- TODO: include data representing debugging hints etc. and rewrite Eq & Ord

-- |The datatype used to represent intermediate type variables.
data Alpha = Alpha Integer
    deriving (Eq, Ord)
    -- TODO: include data representing debugging hints etc. and rewrite Eq & Ord

-- |A wrapper datatype used to represent type variables.
data AnyAlpha = TAlpha Alpha | TUpAlpha UpAlpha
    deriving (Eq, Ord)

-- |The datatype used to represent upper bound types.
data TauUpOpen =
      TuoPrim PrimitiveType
    | TuoFunc UpAlpha Alpha
    | TuoUpAlpha UpAlpha
    deriving (Eq, Ord)

-- |The datatype used to represent types which are either upper bound or
--  intermediate.
data TauUpClosed =
      TucPrim PrimitiveType
    | TucFunc UpAlpha Alpha
    | TucUpAlpha UpAlpha
    | TucAlpha Alpha
    deriving (Eq, Ord)

-- |The datatype used to represent lower bound types.
data TauDownOpen =
      TdoPrim PrimitiveType
    | TdoLabel LabelName TauDownOpen
    | TdoOnion TauDownOpen TauDownOpen
    | TdoFunc (Set AnyAlpha) UpAlpha Alpha Constraints -- TODO: alias Set AnyAlpha?
    deriving (Eq, Ord)

-- |The datatype used to represent types which are either lower bound or
--  intermediate.
data TauDownClosed =
      TdcPrim PrimitiveType
    | TdcLabel LabelName TauDownClosed
    | TdcOnion TauDownClosed TauDownClosed
    | TdcFunc (Set AnyAlpha) UpAlpha Alpha Constraints -- TODO: alias Set AnyAlpha?
    | TdcAlpha Alpha
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
newtype LabelName = LabelName { unLabelName :: String }
    deriving (Eq, Ord)
{- TODO: smarter constructor -}
labelName s = LabelName s

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
--
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

-------------------------------------------------------------------------------
-- *Conversion Type Classes
-- $ConversionTypeClasses
--
-- Type classes used for conversion between the different Haskell types
-- representing Big Bang types.  The conversions to the open forms evaluate to a
-- Maybe type; if a closed tau which contains an intermediate alpha is converted
-- to an open tau, those conversions evaluate to None.

class TauUpOpenConvertible a where
    toTauUpOpen :: a -> Maybe TauUpOpen

class TauUpClosedConvertible a where
    toTauUpClosed :: a -> TauUpClosed

class TauDownOpenConvertible a where
    toTauDownOpen :: a -> Maybe TauDownOpen

class TauDownClosedConvertible a where
    toTauDownClosed :: a -> TauDownClosed

-------------------------------------------------------------------------------
-- *Conversion Type Class Instances
-- $ConversionTypeClassInstances
--
-- Type class instances for conversion between the type forms.

instance TauUpOpenConvertible TauUpOpen where
    toTauUpOpen x = Just x

instance TauUpClosedConvertible TauUpOpen where
    toTauUpClosed x =
        case x of
             TuoPrim p -> TucPrim p
             TuoFunc ua a -> TucFunc ua a
             TuoUpAlpha ua -> TucUpAlpha ua

instance TauUpOpenConvertible TauUpClosed where
    toTauUpOpen x =
        case x of
             TucPrim p -> Just $ TuoPrim p
             TucFunc ua a -> Just $ TuoFunc ua a
             TucUpAlpha ua -> Just $ TuoUpAlpha ua
             TucAlpha a -> Nothing

instance TauUpClosedConvertible TauUpClosed where
    toTauUpClosed x = x

instance TauDownOpenConvertible TauDownOpen where
    toTauDownOpen x = Just x

instance TauDownClosedConvertible TauDownOpen where
    toTauDownClosed x =
        case x of
             TdoPrim p -> TdcPrim p
             TdoLabel lbl t -> TdcLabel lbl (toTauDownClosed t)
             TdoOnion t1 t2 ->
                TdcOnion (toTauDownClosed t1) (toTauDownClosed t2)
             TdoFunc vs ua a c -> TdcFunc vs ua a c

instance TauDownOpenConvertible TauDownClosed where
    toTauDownOpen x = do
        case x of
             TdcPrim p -> return $ TdoPrim p
             TdcLabel lbl t ->
                t' <- toTauDownOpen t
                return $ TdoLabel t'
             TdcOnion t1 t2 ->
                t1' <- toTauDownOpen t1
                t2' <- toTauDownOpen t2
                return $ TdoOnion t1' t2'
             TdcFunc vs ua a c ->
                return TdoFunc vs ua a c
             TdcAlpha a ->
                Nothing

instance TauDownClosedConvertible TauDownClosed where
    toTauDownClosed x = x
