{-# LANGUAGE FlexibleInstances, EmptyDataDecls, GADTs, StandaloneDeriving #-}
module Language.TinyBang.Ast
( Expr(..)
, Chi(..)
, ChiMain
, ChiStruct
, ChiBind
, ChiPrimary
, Branches
, Branch(..)
, Value(..)
-- Re-exported for convenience
, LazyOperator(..)
, EagerOperator(..)
, SubTerm(..)
, exprFromValue
, Assignable(..)
, Evaluated(..)
, CellId
, ePatVars
, exprFreeVars
) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Types.UtilTypes
  ( LabelName
  , Ident
  , unIdent
  , unLabelName
  , LazyOperator(..)
  , EagerOperator(..)
  , SubTerm(..)
  )
import qualified Language.TinyBang.Types.UtilTypes as T
  ( PrimitiveType(..) )
import Utils.Render.Display

-------------------------------------------------------------------------------

type CellId = Int

-- |Data type for representing Big Bang ASTs.
data Expr
  = Var Ident
  | Label LabelName Expr
  | Onion Expr Expr
  | OnionSub Expr SubTerm
  | EmptyOnion
  | Func Ident Expr
  | Appl Expr Expr
  | PrimInt Integer
  | PrimChar Char
  | PrimUnit
  | Case Expr Branches
  | Def Ident Expr Expr
  | Assign Assignable Expr Expr
  | LazyOp LazyOperator Expr Expr
  | EagerOp EagerOperator Expr Expr
  | ExprCell CellId
  deriving (Eq, Ord, Show)

-- |Data type for representing Big Bang values
data Value
  = VLabel LabelName CellId
  | VOnion Value Value
  | VFunc Ident Expr
  | VPrimInt Integer
  | VPrimChar Char
  | VPrimUnit
  | VEmptyOnion
  deriving (Eq, Ord, Show)

data Assignable = ACell CellId | AIdent Ident
  deriving (Eq, Ord, Show)


-- TODO: fix this boilerplate using -XDataKinds in ghc 7.4
data ChiMainType
data ChiStructType
data ChiBindType
data ChiPrimaryType

type ChiMain = Chi ChiMainType
type ChiStruct = Chi ChiStructType
type ChiBind = Chi ChiBindType
type ChiPrimary = Chi ChiPrimaryType

-- |Data type describing top level type patterns in case expressions;
--  corresponds to chi in the document.
data Chi a where
  ChiSimple       :: Maybe Ident -> ChiMain
  ChiComplex      :: ChiStruct   -> ChiMain

  ChiOnionOne     :: ChiBind              -> ChiStruct
  ChiOnionMany    :: ChiBind -> ChiStruct -> ChiStruct

  ChiParen        :: Maybe Ident -> ChiStruct  -> ChiBind
  ChiPrimary      :: Maybe Ident -> ChiPrimary -> ChiBind

  ChiPrim         :: T.PrimitiveType                -> ChiPrimary
  ChiLabelSimple  :: LabelName       -> Maybe Ident -> ChiPrimary
  ChiLabelComplex :: LabelName       -> ChiBind     -> ChiPrimary
  ChiFun          ::                                   ChiPrimary

deriving instance Show (Chi a)
deriving instance Eq (Chi a)
deriving instance Ord (Chi a)

-- -- |Data type describing top level type patterns in case expressions;
-- --  corresponds to chi in the document.
-- data Chi
--   = ChiSimple (Maybe Ident)
--   | ChiComplex ChiStruct
--   deriving (Eq, Ord, Show)

-- -- |Data type describing patterns which impose structure on what they bind;
-- --  corresponds to chi^S in the document.
-- data ChiStruct
--   = ChiBindOne ChiBind
--   | ChiBindMany ChiBind ChiStruct
--   deriving (Eq, Ord, Show)

-- -- |Data type describing patterns which may have final binders attached;
-- --  corresponds to chi^B in the document.
-- data ChiBind
--   = ChiParen (Maybe Ident) ChiStruct
--   | ChiPrimary (Maybe Ident) ChiPrimary
--   deriving (Eq, Ord, Show)

-- -- |Data type describing the base case of patterns;
-- --  corresponds to chi^P in the document.
-- data ChiPrimary
--   = ChiPrim T.PrimitiveType
--   | ChiLabelSimple LabelName (Maybe Ident)
--   | ChiLabelComplex LabelName ChiBind
--   | ChiFun
--   deriving (Eq, Ord, Show)

-- |Alias for case branches
type Branches = [Branch]
data Branch = Branch ChiMain Expr
  deriving (Eq, Ord, Show)

-- |Trivial conversion from values to exprs
-- TODO: deprecate the hell outta this thing
exprFromValue :: (Evaluated v) => v -> Expr
exprFromValue v = case value v of
  VLabel l c   -> Label l $ ExprCell c
  VOnion v1 v2 -> Onion (exprFromValue v1) (exprFromValue v2)
  VFunc i e    -> Func i e
  VPrimInt i   -> PrimInt i
  VPrimChar c  -> PrimChar c
  VPrimUnit    -> PrimUnit
  VEmptyOnion  -> EmptyOnion

-- TODO: refactor the pattern stuff into its own module?
-- |Obtains the set of bound variables in a pattern.
ePatVars :: Chi a -> Set Ident
ePatVars chi =
  case chi of
    ChiSimple mx -> maybeIdent mx
    ChiComplex chiStruct -> ePatVars chiStruct
    ChiOnionOne chiBind -> ePatVars chiBind
    ChiOnionMany chiBind chiStruct -> both chiBind chiStruct
    ChiParen mx chiStruct -> varAndMore mx chiStruct
    ChiPrimary mx chiPrimary -> varAndMore mx chiPrimary
    ChiPrim _ -> Set.empty
    ChiLabelSimple _ mx -> maybeIdent mx
    ChiLabelComplex _ chiBind -> ePatVars chiBind
    ChiFun -> Set.empty
  where maybeIdent mx = maybe Set.empty Set.singleton mx
        both :: Chi a -> Chi b -> Set Ident
        both x y = Set.union (ePatVars y) (ePatVars x)
        varAndMore :: Maybe Ident -> Chi a -> Set Ident
        varAndMore mx chi' = Set.union (ePatVars chi') (maybeIdent mx)

-- |Obtains the set of free variables for a given expression.
exprFreeVars :: Expr -> Set Ident
exprFreeVars e =
  case e of
    Var i -> Set.singleton i
    Label _ e' -> exprFreeVars e'
    Onion e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2
    Func i e' -> i `Set.delete` exprFreeVars e'
    Appl e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2
    PrimInt _ -> Set.empty
    PrimChar _ -> Set.empty
    PrimUnit -> Set.empty
    Case e' brs -> Set.union (exprFreeVars e') $ Set.unions $
      map (\(Branch chi e'') ->
              exprFreeVars e'' `Set.difference` ePatVars chi) brs
    OnionSub e' _ -> exprFreeVars e'
    EmptyOnion -> Set.empty
    LazyOp _ e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2 
    EagerOp _ e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2
    Def i e1 e2 -> (i `Set.delete` exprFreeVars e2) `Set.union` exprFreeVars e1
    Assign a e1 e2 ->
        ((case a of
            AIdent i -> (Set.delete i)
            ACell _ -> id) $ exprFreeVars e2)
          `Set.union` exprFreeVars e1
    ExprCell _ -> Set.empty

instance Display Expr where
  makeDoc a = case a of
    Var i -> text $ unIdent i
    Label n e -> char '`' <> (text $ unLabelName n) <+> makeDoc e
    Onion e1 e2 -> makeDoc e1 <+> char '&' <+> makeDoc e2
    Func i e -> parens $
            text "fun" <+> (text $ unIdent i) <+> text "->" <+> makeDoc e
    Appl e1 e2 -> parens $ makeDoc e1 <+> makeDoc e2
    PrimInt i -> integer i
    PrimChar c -> quotes $ char c
    PrimUnit -> parens empty
    Case e brs -> text "case" <+> makeDoc e <+> text "of" <+> text "{" $+$
            (nest indentSize $ vcat $ punctuate semi $ map makeDoc brs)
            $+$ text "}"
    OnionSub e s -> makeDoc e <+> char '&' <> makeDoc s
    EmptyOnion -> text "(&)"
    LazyOp op e1 e2 -> makeDoc op <+> makeDoc e1 <+> makeDoc e2
    EagerOp op e1 e2 -> makeDoc op <+> makeDoc e1 <+> makeDoc e2
    Def i v e -> hsep [text "def", makeDoc i, text "=", makeDoc v, text "in", makeDoc e]
    Assign i v e -> hsep [makeDoc i, text "=", makeDoc v, text "in", makeDoc e]
    ExprCell c -> text "Cell #" <> int c

instance Display Value where
  makeDoc x =
    case x of
      VEmptyOnion -> text "(&)"
      _ -> makeDoc $ exprFromValue x

instance Display Branch where
  makeDoc (Branch chi e) =
    makeDoc chi <+> text "->" <+> makeDoc e

instance Display (Chi a) where
  makeDoc chi =
    case chi of
      ChiSimple mx -> docDummyMx mx
      ChiComplex chiStruct -> makeDoc chiStruct
      ChiOnionOne chiBind -> makeDoc chiBind
      ChiOnionMany chiBind chiStruct ->
        makeDoc chiBind <+> text "&" <+> makeDoc chiStruct
      ChiParen mx chiStruct ->
        docPreMx mx <+> makeDoc chiStruct
      ChiPrimary mx chiPri ->
        docPreMx mx <+> makeDoc chiPri
      ChiPrim p -> makeDoc p
      ChiLabelSimple lbl mx -> makeDoc lbl <+> docDummyMx mx
      ChiLabelComplex lbl chiBind -> makeDoc lbl <+> makeDoc chiBind
      ChiFun -> text "fun"
    where docDummyMx mx = text $ maybe "_" unIdent mx
          docPreMx mx = maybe empty (text . (++":") . unIdent) mx

instance Display Assignable where
  makeDoc (AIdent i) = makeDoc i
  makeDoc (ACell v) = makeDoc v

class Evaluated a where
  value :: a -> Value
  value v = fst $ vmPair v
  mapping :: a -> IntMap Value
  mapping v = snd $ vmPair v

  vmPair :: a -> (Value, IntMap Value)
  vmPair v = (value v, mapping v)

instance Evaluated (Value, IntMap Value) where
  vmPair = id

instance Evaluated Value where
  value = id
  mapping = const IntMap.empty
