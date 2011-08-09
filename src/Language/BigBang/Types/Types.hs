module Language.BigBang.Types.Types 
( AlphaContents(..)
, AlphaUp(..)
, Alpha(..)
, AnyAlpha(..)
, construct
, getIndex
, CallSite(..)
, getCallSites
, callSites
, unCallSites
, TauUpOpen(..)
, TauUpClosed(..)
, TauDownOpen(..)
, TauDownClosed(..)
, PolyFuncData(..)
, PrimitiveType(..)
, TauChi(..)
, Constraints
, Constraint(..)
, Guard(..)
, toSomeAlpha
, toAnyAlpha
, toTauUpOpen
, toTauUpClosed
, toTauDownOpen
, toTauDownClosed
, (<:)
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Language.BigBang.Types.UtilTypes (LabelName)
import Language.BigBang.Render.Display

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

-- |A data structure representing a single call site.
data CallSite = CallSite (Set AlphaUp)
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
-- TODO: are we actually using reverse order?  Resolve this!
newtype CallSites = CallSites { unCallSites :: [CallSite] }
    deriving (Eq, Ord, Show)
callSites :: [CallSite] -> CallSites
callSites lst = CallSites lst

-- |The datatype used to represent upper bound types.
data TauUpOpen =
      TuoPrim PrimitiveType
    | TuoFunc AlphaUp Alpha
    | TuoTop
    | TuoAlphaUp AlphaUp
    deriving (Eq, Ord, Show)

-- |The datatype used to represent types which are either upper bound or
--  intermediate.
data TauUpClosed =
      TucPrim PrimitiveType
    | TucFunc AlphaUp Alpha
    | TucTop
    | TucAlphaUp AlphaUp
    | TucAlpha Alpha
    deriving (Eq, Ord, Show)

-- |The datatype used to represent lower bound types.
data TauDownOpen =
      TdoPrim PrimitiveType
    | TdoLabel LabelName TauDownOpen
    | TdoOnion TauDownOpen TauDownOpen
    | TdoFunc PolyFuncData
    | TdoTop
    deriving (Eq, Ord, Show)

-- |The datatype used to represent types which are either lower bound or
--  intermediate.
data TauDownClosed =
      TdcPrim PrimitiveType
    | TdcLabel LabelName TauDownClosed
    | TdcOnion TauDownClosed TauDownClosed
    | TdcFunc PolyFuncData
    | TdcTop
    | TdcAlpha Alpha
    deriving (Eq, Ord, Show)

-- |A wrapper type containing the polymorphic function type information.
data PolyFuncData =
    PolyFuncData (Set AnyAlpha) Alpha Alpha Constraints -- TODO: alias Set AnyAlpha?
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
    | ChiFun
    | ChiTop
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

-- |A type class for converting types (such as TauDownClosed) to AnyAlphas
class AlphaConvertible a where
    toSomeAlpha :: a -> Maybe AnyAlpha

-- |A type class for converting type variables (such as AlphaUp) to AnyAlphas
class AnyAlphaConvertible a where
    toAnyAlpha :: a -> AnyAlpha

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

instance AlphaConvertible TauUpOpen where
    toSomeAlpha = const Nothing

instance TauUpOpenConvertible TauUpClosed where
    toTauUpOpen x =
        case x of
             TucPrim p -> Just $ TuoPrim p
             TucFunc ua a -> Just $ TuoFunc ua a
             TucAlphaUp ua -> Just $ TuoAlphaUp ua
             TucAlpha a -> Nothing

instance TauUpClosedConvertible TauUpClosed where
    toTauUpClosed x = x

instance AlphaConvertible TauUpClosed where
    toSomeAlpha x =
        case x of
            TucAlpha a -> Just $ SomeAlpha a
            TucAlphaUp a -> Just $ SomeAlphaUp a
            _ -> Nothing

instance TauDownOpenConvertible TauDownOpen where
    toTauDownOpen x = Just x

instance TauDownClosedConvertible TauDownOpen where
    toTauDownClosed x =
        case x of
             TdoPrim p -> TdcPrim p
             TdoLabel lbl t -> TdcLabel lbl (toTauDownClosed t)
             TdoOnion t1 t2 ->
                TdcOnion (toTauDownClosed t1) (toTauDownClosed t2)
             TdoFunc pfd -> TdcFunc pfd

instance AlphaConvertible TauDownOpen where
    toSomeAlpha = const Nothing

instance TauDownOpenConvertible TauDownClosed where
    toTauDownOpen x = do
        case x of
             TdcPrim p -> return $ TdoPrim p
             TdcTop -> return TdoTop
             TdcLabel lbl t -> do
                t' <- toTauDownOpen t
                return $ TdoLabel lbl t'
             TdcOnion t1 t2 -> do
                t1' <- toTauDownOpen t1
                t2' <- toTauDownOpen t2
                return $ TdoOnion t1' t2'
             TdcFunc pfd ->
                return $ TdoFunc pfd
             TdcAlpha _ ->
                Nothing

instance TauDownClosedConvertible TauDownClosed where
    toTauDownClosed x = x

instance AlphaConvertible TauDownClosed where
    toSomeAlpha x =
        case x of
            TdcAlpha a -> Just $ SomeAlpha a
            _ -> Nothing

instance AnyAlphaConvertible Alpha where
    toAnyAlpha = SomeAlpha

instance AnyAlphaConvertible AlphaUp where
    toAnyAlpha = SomeAlphaUp

-------------------------------------------------------------------------------
-- *Projection Type Classes
-- $ProjectionTypeClasses
--
-- Type classes to simplify partial destruction of Big Bang type
-- representations.

class TAlpha a where
    getIndex :: a -> Integer
    getCallSites :: a -> CallSites
    construct :: Integer -> CallSites -> a

-------------------------------------------------------------------------------
-- *Projection Type Class Instances
-- $ProjectionTypeClassInstances
--
-- Implementations of projection type classes for type representations.

instance TAlpha AlphaContents where
    getIndex (AlphaContents i _) = i
    getCallSites (AlphaContents _ c) = c
    construct i c = AlphaContents i c

instance TAlpha Alpha where
    getIndex (Alpha c) = getIndex c
    getCallSites (Alpha c) = getCallSites c
    construct i c = Alpha (AlphaContents i c)

instance TAlpha AlphaUp where
    getIndex (AlphaUp c) = getIndex c
    getCallSites (AlphaUp c) = getCallSites c
    construct i c = AlphaUp (AlphaContents i c)

-------------------------------------------------------------------------------
-- *Display Type Classes
-- $DisplayTypeClasses
--
-- Implementations of display routines for type structures.

instance Display AlphaContents where
    makeDoc (AlphaContents i callSites) =
        char '\'' <> makeDoc i <> (
            let doc = makeDoc callSites in
            if not $ isEmpty doc
                then char '^' <> doc
                else empty)

instance Display AlphaUp where
    makeDoc (AlphaUp ac) = makeDoc ac

instance Display Alpha where
    makeDoc (Alpha ac) = makeDoc ac

instance Display AnyAlpha where
    makeDoc a = case a of
        SomeAlpha b -> makeDoc b
        SomeAlphaUp b -> makeDoc b

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
      
instance Display TauUpOpen where
    makeDoc tau = makeDoc $ toTauUpClosed tau

instance Display TauUpClosed where
    makeDoc tau = case tau of
        TucPrim p -> makeDoc p
        TucFunc au a -> makeDoc au <+> text "->" <+> makeDoc a
        TucTop -> text "top"
        TucAlphaUp a -> makeDoc a
        TucAlpha a -> makeDoc a

instance Display TauDownOpen where
    makeDoc tau = makeDoc $ toTauDownClosed tau

instance Display TauDownClosed where
    makeDoc tau = case tau of
        TdcPrim p -> makeDoc p
        TdcLabel n t -> char '`' <> makeDoc n <+> makeDoc t
        TdcOnion t1 t2 -> makeDoc t1 <+> char '&' <+> makeDoc t2
        TdcFunc polyFuncData -> makeDoc polyFuncData
        TdcTop -> text "top"
        TdcAlpha a -> makeDoc a

instance Display PolyFuncData where
    makeDoc (PolyFuncData alphas alpha1 alpha2 constraints) =
        (if Set.size alphas > 0
            then text "all" <+> (parens $ makeDoc alphas)
            else empty) <+>
        makeDoc alpha1 <+> text "->" <+> makeDoc alpha2 <+>
        char '\\' <+> (parens $ makeDoc constraints)

instance Display PrimitiveType where
    makeDoc p = case p of
        PrimInt -> text "int"
        PrimChar -> text "char"
        PrimUnit -> text "unit"

instance Display TauChi where
    makeDoc tauChi = case tauChi of
        ChiPrim p -> makeDoc p
        ChiLabel n au -> char '`' <> makeDoc n <+> makeDoc au
        ChiFun -> text "fun"
        ChiTop -> text "_"

instance Display Constraint where
    makeDoc c = case c of
        Subtype a b -> makeDoc a <+> text "<:" <+> makeDoc b
        Case alphaUp guards ->
                    text "case" <+> makeDoc alphaUp <+> text "of" <+> lbrace $+$
                    (nest indentSize $ vcat $ punctuate semi $ map gDoc guards)
                    $+$ rbrace
        Bottom -> text "_|_"
      where gDoc (Guard tauChi constraints) =
                makeDoc tauChi <+> text "->" <+> makeDoc constraints
