{-# LANGUAGE GADTs #-}

module Types 
(
) where

class Tau a
class (Tau a) => TauOpen a
class (Tau a, TauOpen a) => TauClosed a
class (Tau a) => TauDown a
class (Tau a) => TauUp a
class (TauDown a) => TauLabel a
class (TauDown a) => TauOnion a
class (TauDown a, TauUp a) => TauPrim a
class (Tau a) => TauAlpha a
class (TauOpen a, TauDown a) => TauDownOpen a
class (TauOpen a, TauUp a) => TauUpOpen a
class (TauClosed a, TauDown a) => TauDownClosed a
class (TauClosed a, TauUp a) => TauUpClosed a

newtype Label = Label { unLabel :: String }
{- TODO: smarter constructor -}
label s = Label s

data TauChi where
    ChiPrim :: (TauPrim a) => a -> TauChi
    ChiLabel :: Label -> Alpha -> TauChi
    ChiOnion :: Alpha -> Alpha -> TauChi
    ChiFun :: Alpha -> TauChi

data OpenLabel where
    OpenLabel :: (TauOpen a) => Label -> a -> OpenLabel
data ClosedLabel where
    ClosedLabel :: (TauClosed a) => Label -> a -> ClosedLabel
data OpenOnion where
    OpenOnion :: (TauOpen a) => a -> a -> OpenOnion
data ClosedOnion where
    ClosedOnion :: (TauClosed a) => a -> a -> ClosedOnion
data PrimInt where
    PrimInt :: PrimInt
data PrimString where
    PrimString :: PrimString
data PrimUnit where
    PrimUnit :: PrimUnit
data PolyFunc where
    PolyFunc :: (TauAlpha a) =>
        [a] -> AlphaUp -> Alpha -> Constraints -> PolyFunc
data Alpha where
    Alpha :: Int -> Alpha
data AlphaUp where
    AlphaUp :: Int -> AlphaUp
data UpperFunc where
    UpperFunc :: AlphaUp -> Alpha -> UpperFunc

type Constraints = [Constraint]
data Constraint where
    Subtype :: (TauDownClosed a, TauUpClosed b) => a -> b -> Constraint
    Case :: AlphaUp -> [Guard] -> Constraint
    Bottom :: Constraint
data Guard = Guard TauChi Constraints


instance Tau OpenLabel
instance TauOpen OpenLabel
instance TauLabel OpenLabel
instance TauDown OpenLabel
instance TauDownOpen OpenLabel

instance Tau ClosedLabel
instance TauOpen ClosedLabel
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
instance TauOpen ClosedOnion
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
