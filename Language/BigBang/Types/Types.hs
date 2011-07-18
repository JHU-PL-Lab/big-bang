module Language.BigBang.Types.Types 
( AlphaContents(..)
, AlphaUp(..)
, Alpha(..)
, AnyAlpha(..)
, construct
, index
, callSites
, TauUpOpen(..)
, TauUpClosed(..)
, TauDownOpen(..)
, TauDownClosed(..)
, PrimitiveType(..)
, TauChi(..)
, Constraints
, Constraint(..)
, Guard(..)
, toTauUpOpen
, toTauUpClosed
, toTauDownOpen
, toTauDownClosed
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.BigBang.Types.UtilTypes (LabelName)

-------------------------------------------------------------------------------
-- *Big Bang Types
-- $BigBangTypes
--
-- These data types are used to represent Big Bang's type grammar.

-- |A datatype used to represent the common contents of all type variables.
data AlphaContents = AlphaContents Integer CallSites
    deriving (Eq, Ord, Show)
    -- TODO: include data representing debugging hints etc. and rewrite Eq & Ord

-- |The datatype used to represent upper bound type variables.
data AlphaUp = AlphaUp AlphaContents
    deriving (Eq, Ord, Show)
    -- TODO: include data representing debugging hints etc. and rewrite Eq & Ord

-- |The datatype used to represent intermediate type variables.
data Alpha = Alpha AlphaContents
    deriving (Eq, Ord, Show)
    -- TODO: include data representing debugging hints etc. and rewrite Eq & Ord

-- |A wrapper datatype used to represent type variables.
data AnyAlpha = SomeAlpha Alpha | SomeAlphaUp AlphaUp
    deriving (Eq, Ord, Show)

-- |A data structure representing function call sites.  The call site of a
--  function application is defined by the type parameter used as the domain
--  of the function type upper bounding the function in that application.
data CallSite = CallSite AlphaUp (Maybe AlphaUp)
    deriving (Eq, Ord, Show)
type CallSites = Set CallSite

-- |The datatype used to represent upper bound types.
data TauUpOpen =
      TuoPrim PrimitiveType
    | TuoFunc AlphaUp Alpha
    | TuoAlphaUp AlphaUp
    deriving (Eq, Ord, Show)

-- |The datatype used to represent types which are either upper bound or
--  intermediate.
data TauUpClosed =
      TucPrim PrimitiveType
    | TucFunc AlphaUp Alpha
    | TucAlphaUp AlphaUp
    | TucAlpha Alpha
    deriving (Eq, Ord, Show)

-- |The datatype used to represent lower bound types.
data TauDownOpen =
      TdoPrim PrimitiveType
    | TdoLabel LabelName TauDownOpen
    | TdoOnion TauDownOpen TauDownOpen
    | TdoFunc (Set AnyAlpha) AlphaUp Alpha Constraints -- TODO: alias Set AnyAlpha?
    deriving (Eq, Ord, Show)

-- |The datatype used to represent types which are either lower bound or
--  intermediate.
data TauDownClosed =
      TdcPrim PrimitiveType
    | TdcLabel LabelName TauDownClosed
    | TdcOnion TauDownClosed TauDownClosed
    | TdcFunc (Set AnyAlpha) AlphaUp Alpha Constraints -- TODO: alias Set AnyAlpha?
    | TdcAlpha Alpha
    deriving (Eq, Ord, Show)

-- |The datatype enumerating the primitives in the Big Bang type system.
data PrimitiveType =
      PrimInt
    | PrimChar
    | PrimUnit
    deriving (Eq, Ord, Show)


-------------------------------------------------------------------------------
-- *Type Pattern Types
-- $TypePatternTypes
--
-- These types are used to describe type patterns in Big Bang.

-- |A type representing the patterns produced by guards.
data TauChi =
      ChiPrim PrimitiveType
    | ChiLabel LabelName Alpha
    | ChiOnion Alpha Alpha
    | ChiFun Alpha
    deriving (Eq, Ord, Show)

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
    | Case AlphaUp [Guard]
    | Bottom
    deriving (Eq, Ord, Show)
-- |A type representing guards in Big Bang case constraints.
data Guard = Guard TauChi Constraints
    deriving (Eq, Ord, Show)

-- |An infix function for creating subtype contraints (for convenience).
(<:) :: TauDownClosed -> TauUpClosed -> Constraint
(<:) = Subtype

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
             TuoAlphaUp ua -> TucAlphaUp ua

instance TauUpOpenConvertible TauUpClosed where
    toTauUpOpen x =
        case x of
             TucPrim p -> Just $ TuoPrim p
             TucFunc ua a -> Just $ TuoFunc ua a
             TucAlphaUp ua -> Just $ TuoAlphaUp ua
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
             TdcLabel lbl t -> do
                t' <- toTauDownOpen t
                return $ TdoLabel lbl t'
             TdcOnion t1 t2 -> do
                t1' <- toTauDownOpen t1
                t2' <- toTauDownOpen t2
                return $ TdoOnion t1' t2'
             TdcFunc vs ua a c ->
                return $ TdoFunc vs ua a c
             TdcAlpha a ->
                Nothing

instance TauDownClosedConvertible TauDownClosed where
    toTauDownClosed x = x

-------------------------------------------------------------------------------
-- *Projection Type Classes
-- $ProjectionTypeClasses
--
-- Type classes to simplify partial destruction of Big Bang type
-- representations.

class TAlpha a where
    index :: a -> Integer
    callSites :: a -> CallSites
    construct :: Integer -> CallSites -> a

-------------------------------------------------------------------------------
-- *Projection Type Class Instances
-- $ProjectionTypeClassInstances
--
-- Implementations of projection type classes for type representations.

instance TAlpha AlphaContents where
    index (AlphaContents i _) = i
    callSites (AlphaContents _ c) = c
    construct i c = AlphaContents i c

instance TAlpha Alpha where
    index (Alpha c) = index c
    callSites (Alpha c) = callSites c
    construct i c = Alpha (AlphaContents i c)

instance TAlpha AlphaUp where
    index (AlphaUp c) = index c
    callSites (AlphaUp c) = callSites c
    construct i c = AlphaUp (AlphaContents i c)
