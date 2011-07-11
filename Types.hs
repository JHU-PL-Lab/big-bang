{-# LANGUAGE GADTs #-}

module Types 
(
) where

-------------------------------------------------------------------------------
-- Declaring type classes to categorize the different concrete types used to
-- represent type values in the Big Bang system.

-- |The supertype of all values which represent Big Bang types.
class Tau a
-- |The supertype of all "open" types in the notation.  Open types are those
--  which never contain intermediate type variables.
class (Tau a) => TauOpen a
-- |The supertype of all "closed" types in the notation.  Closed types are those
--  which might contain intermediate type variables.
class (Tau a) => TauClosed a
-- |The supertype of all types representing concrete lower bounds.
class (Tau a) => TauDown a
-- |The supertype of all types representing concrete upper bounds.
class (Tau a) => TauUp a
-- |The supertype of all types representing labels.
class (TauDown a) => TauLabel a
-- |The supertype of all types representing onions.
class (TauDown a) => TauOnion a
-- |The supertype of all types representing primitives.
class (TauDown a, TauUp a) => TauPrim a
-- |The supertype of all types representing all type variables.
class (Tau a) => TauAlpha a
-- |The supertype of all types which are both lower bounds and open.
class (TauOpen a, TauDown a) => TauDownOpen a
-- |The supertype of all types which are both upper bounds and open.
class (TauOpen a, TauUp a) => TauUpOpen a
-- |The supertype of all types which are both lower bounds and closed.
class (TauClosed a, TauDown a) => TauDownClosed a
-- |The supertype of all types which are both upper bounds and closed.
class (TauClosed a, TauUp a) => TauUpClosed a

------------------------------------------------------------------------------
-- Declaring types in preparation for the concrete types below.

-- |A distinguished type for labels.
newtype Label = Label { unLabel :: String }
{- TODO: smarter constructor -}
label s = Label s

-- |A type representing the patterns produced by guards.
data TauChi where
    ChiPrim :: (TauPrim a) => a -> TauChi
    ChiLabel :: Label -> Alpha -> TauChi
    ChiOnion :: Alpha -> Alpha -> TauChi
    ChiFun :: Alpha -> TauChi

------------------------------------------------------------------------------
-- Concrete types representing types in Big Bang

-- |A concrete type representing deeply open labels.
data OpenLabel where
    OpenLabel :: (TauOpen a) => Label -> a -> OpenLabel
-- |A concrete type representing deeply closed labels.
data ClosedLabel where
    ClosedLabel :: (TauClosed a) => Label -> a -> ClosedLabel
-- |A concrete type representing deeply open onions.
data OpenOnion where
    OpenOnion :: (TauOpen a) => a -> a -> OpenOnion
-- |A concrete type representing deeply closed onions.
data ClosedOnion where
    ClosedOnion :: (TauClosed a) => a -> a -> ClosedOnion
-- |A concrete type representing primitive integers.
data PrimInt where
    PrimInt :: PrimInt
-- |A concrete type representing primitive string.
data PrimString where
    PrimString :: PrimString
-- |A concrete type representing primitive unit.
data PrimUnit where
    PrimUnit :: PrimUnit
-- |A concrete type representing a polymorphic function.  The arguments are, in
--  order, a list of type variables over which the function is polymorphic, the
--  input type of the function, the output type of the function, and the
--  constraints applied to the function type.
data PolyFunc where
    PolyFunc :: (TauAlpha a) =>
        [a] -> AlphaUp -> Alpha -> Constraints -> PolyFunc
-- |A concrete type representing intermediate type variables.
data Alpha where
    Alpha :: Int -> Alpha
-- |A concrete type representing upper-bounding type variables.
data AlphaUp where
    AlphaUp :: Int -> AlphaUp
-- |A concrete type representing function types as upper bounds.
data UpperFunc where
    UpperFunc :: AlphaUp -> Alpha -> UpperFunc

------------------------------------------------------------------------------
-- Declaring types to represent Big Bang constraints.

-- |A type alias defining the form of constraints.
type Constraints = [Constraint]
-- |A type describing constraints in Big Bang.
data Constraint where
    Subtype :: (TauDownClosed a, TauUpClosed b) => a -> b -> Constraint
    Case :: AlphaUp -> [Guard] -> Constraint
    Bottom :: Constraint
-- |A type representing guards in Big Bang case constraints.
data Guard = Guard TauChi Constraints

------------------------------------------------------------------------------
-- Declaring type class membership of the concrete types defined above.

instance Tau OpenLabel
instance TauOpen OpenLabel
instance TauLabel OpenLabel
instance TauDown OpenLabel
instance TauDownOpen OpenLabel

instance Tau ClosedLabel
instance TauClosed ClosedLabel
instance TauLabel ClosedLabel
instance TauDown ClosedLabel
instance TauDownClosed ClosedLabel

instance Tau OpenOnion
instance TauOpen OpenOnion
instance TauOnion OpenOnion
instance TauDown OpenOnion
instance TauDownOpen OpenOnion

instance Tau ClosedOnion
instance TauClosed ClosedOnion
instance TauOnion ClosedOnion
instance TauDown ClosedOnion
instance TauDownClosed ClosedOnion

instance Tau PrimInt
instance TauUp PrimInt
instance TauDown PrimInt
instance TauPrim PrimInt

instance Tau PrimString
instance TauUp PrimString
instance TauDown PrimString
instance TauPrim PrimString

instance Tau PrimUnit
instance TauUp PrimUnit
instance TauDown PrimUnit
instance TauPrim PrimUnit
instance TauOpen PrimUnit

instance Tau PolyFunc
instance TauDown PolyFunc
instance TauOpen PolyFunc
instance TauClosed PolyFunc

instance Tau Alpha
instance TauAlpha Alpha

instance Tau AlphaUp
instance TauAlpha AlphaUp
instance TauUp AlphaUp
instance TauOpen AlphaUp

instance Tau UpperFunc
instance TauUp UpperFunc
instance TauOpen UpperFunc
instance TauClosed UpperFunc

