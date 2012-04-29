{-# LANGUAGE      FlexibleInstances
                , FlexibleContexts
                , EmptyDataDecls
                , GADTs
                , StandaloneDeriving
                , MultiParamTypeClasses
                , TypeSynonymInstances
                #-}
module Language.TinyBang.Ast
( Expr
, ExprPart(..)
, Modifier(..)
, Chi(..)
, ChiMain
, ChiStruct
, ChiBind
, ChiPrimary
, ChiMainType
, ChiStructType
, ChiBindType
, ChiPrimaryType
, Branches
, Branch(..)
, Value(..)
, Evaluated(..)
, CellId
, ePatVars
, exprVars
, VarsOp(..)
, exprFreeVars
, FreeVarsOp(..)
, subst
, SubstOp(..)
-- Re-exported for convenience
, LazyOperator(..)
, EagerOperator(..)
, ProjTerm(..)
, Assignable(..)
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
  , ProjTerm(..)
  )
import qualified Language.TinyBang.Types.UtilTypes as T
  ( PrimitiveType(..) )
import Utils.Language.Ast
import Utils.Render.Display

-------------------------------------------------------------------------------

type CellId = Int

-- TODO: separate cell AST nodes into a different structure
-- |Data type for representing TinyBang ASTs.
type Expr = Ast1 ExprPart

-- |Data type for representing TinyBang AST nodes.
data ExprPart t
  = Var Ident
  | Label LabelName (Maybe Modifier) t
  | Onion t t
  | OnionSub t ProjTerm
  | OnionProj t ProjTerm
  | EmptyOnion
  | Func Ident t
  | Appl t t
  | PrimInt Integer
  | PrimChar Char
  | PrimUnit
  | Case t (Branches t)
  | Def (Maybe Modifier) Ident t t
  | Assign Assignable t t
  | LazyOp LazyOperator t t
  | EagerOp EagerOperator t t
  | ExprCell CellId
  deriving (Eq, Ord, Show)

data Modifier
  = Final
  | Immutable
  deriving (Eq, Ord, Show, Enum)

-- |Data type for representing evaluated ASTs.  The type parameter indicates
--  the original type of AST which was evaluated (for function bodies).
data Value t
  = VLabel LabelName CellId
  | VOnion (Value t) (Value t)
  | VFunc Ident t
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
  ChiTopVar       :: Ident                   -> ChiMain
  ChiTopOnion     :: ChiPrimary -> ChiStruct -> ChiMain
  ChiTopBind      :: ChiBind                 -> ChiMain

  ChiOnionMany    :: ChiPrimary -> ChiStruct -> ChiStruct
  ChiOnionOne     :: ChiPrimary              -> ChiStruct

  ChiBound        :: Ident -> ChiBind -> ChiBind
  ChiUnbound      :: ChiPrimary       -> ChiBind

  ChiPrim         :: T.PrimitiveType                -> ChiPrimary
  ChiLabelShallow :: LabelName       -> Ident       -> ChiPrimary
  ChiLabelDeep    :: LabelName       -> ChiBind     -> ChiPrimary
  ChiFun          ::                                   ChiPrimary
  ChiInnerStruct  :: ChiStruct                      -> ChiPrimary

deriving instance Show (Chi a)
deriving instance Eq (Chi a)
deriving instance Ord (Chi a)

-- |Alias for case branches
type Branches t = [Branch t]
data Branch t = Branch ChiMain t
  deriving (Eq, Ord, Show)

-- TODO: refactor the pattern stuff into its own module?
-- |Obtains the set of bound variables in a pattern.
ePatVars :: Chi a -> Set Ident
ePatVars chi =
  case chi of
    ChiTopVar x -> Set.singleton x
    ChiTopOnion p s -> both p s
    ChiTopBind b -> ePatVars b
    ChiOnionMany p s -> both p s
    ChiOnionOne p -> ePatVars p
    ChiBound i b -> Set.insert i $ ePatVars b
    ChiUnbound p -> ePatVars p
    ChiPrim _ -> Set.empty
    ChiLabelShallow _ x -> Set.singleton x
    ChiLabelDeep _ b -> ePatVars b
    ChiFun -> Set.empty
    ChiInnerStruct s -> ePatVars s
  where both :: Chi a -> Chi b -> Set Ident
        both x y = Set.union (ePatVars y) (ePatVars x)

-- |Obtains the set of free variables for an AST containing TinyBang nodes.
exprFreeVars :: (AstOp FreeVarsOp ast (Set Ident)) => ast -> Set Ident
exprFreeVars = astop FreeVarsOp
data FreeVarsOp = FreeVarsOp
instance (AstOp FreeVarsOp ast (Set Ident))
      => AstStep FreeVarsOp ExprPart ast (Set Ident) where
  aststep FreeVarsOp ast = case ast of
    Var i -> Set.singleton i
    Label _ _ e' -> exprFreeVars e'
    Onion e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2
    OnionProj e' _ -> exprFreeVars e'
    OnionSub e' _ -> exprFreeVars e'
    Func i e' -> i `Set.delete` exprFreeVars e'
    Appl e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2
    PrimInt _ -> Set.empty
    PrimChar _ -> Set.empty
    PrimUnit -> Set.empty
    Case e' brs -> Set.union (exprFreeVars e') $ Set.unions $
      map (\(Branch chi e'') ->
              exprFreeVars e'' `Set.difference` ePatVars chi) brs
    EmptyOnion -> Set.empty
    LazyOp _ e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2
    EagerOp _ e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2
    Def _ i e1 e2 ->
      (i `Set.delete` exprFreeVars e2) `Set.union` exprFreeVars e1
    Assign a e1 e2 ->
        ((case a of
            AIdent i -> (Set.delete i)
            ACell _ -> id) $ exprFreeVars e2)
          `Set.union` exprFreeVars e1
    ExprCell _ -> Set.empty

-- |Obtains the set of all variables in a given expression.  This includes the
--  variables found in patterns and other constructs.
exprVars :: (AstOp VarsOp ast (Set Ident)) => ast -> Set Ident
exprVars = astop VarsOp
data VarsOp = VarsOp
instance (AstOp VarsOp ast (Set Ident))
      => AstStep VarsOp ExprPart ast (Set Ident) where
  aststep VarsOp ast = case ast of
    Var i -> Set.singleton i
    Label _ _ e' -> exprVars e'
    Onion e1 e2 -> exprVars e1 `Set.union` exprVars e2
    OnionProj e' _ -> exprVars e'
    OnionSub e' _ -> exprVars e'
    Func i e' -> i `Set.insert` exprVars e'
    Appl e1 e2 -> exprVars e1 `Set.union` exprVars e2
    PrimInt _ -> Set.empty
    PrimChar _ -> Set.empty
    PrimUnit -> Set.empty
    Case e' brs -> Set.union (exprVars e') $ Set.unions $
      map (\(Branch chi e'') ->
              exprVars e'' `Set.difference` ePatVars chi) brs
    EmptyOnion -> Set.empty
    LazyOp _ e1 e2 -> exprVars e1 `Set.union` exprVars e2
    EagerOp _ e1 e2 -> exprVars e1 `Set.union` exprVars e2
    Def _ i e1 e2 -> (i `Set.insert` exprVars e1) `Set.union` exprVars e2
    Assign a e1 e2 -> (case a of
                        AIdent i -> Set.insert i
                        ACell _ -> id) $ exprVars e1 `Set.union` exprVars e2
    ExprCell _ -> Set.empty

-- |Performs a free variable substitution on the provided TinyBang AST.
subst :: (AstWrap ExprPart ast
        , AstOp SubstOp ast (ExprPart ast -> Ident -> ast))
      => ast            -- ^The AST
      -> ExprPart ast   -- ^The substituting expression
      -> Ident          -- ^The identifier to substitute
      -> ast            -- ^The resulting AST
subst = astop SubstOp
data SubstOp = SubstOp
instance (AstWrap ExprPart ast
        , AstOp SubstOp ast (ExprPart ast -> Ident -> ast))
      => AstStep SubstOp ExprPart ast (ExprPart ast -> Ident -> ast) where
  aststep SubstOp orig sub ident = astwrap $ case orig of
    Var i | i == ident -> sub
    Var _ -> orig
    Label n m e -> Label n m $ rec e
    Onion e1 e2 -> Onion (rec e1) (rec e2)
    OnionSub e s -> OnionSub (rec e) s
    OnionProj e s -> OnionProj (rec e) s
    Func i _ | i == ident -> orig
    Func i e -> Func i $ rec e
    Appl e1 e2 -> Appl (rec e1) (rec e2)
    PrimInt _ -> orig
    PrimChar _ -> orig
    PrimUnit -> orig
    Case e branches -> Case (rec e) $
        map (\(Branch pat bre) -> Branch pat $
          (if ident `Set.member` ePatVars pat then id else rec) bre) branches
    EmptyOnion -> orig
    LazyOp op e1 e2 -> LazyOp op (rec e1) (rec e2)
    EagerOp op e1 e2 -> EagerOp op (rec e1) (rec e2)
    Def _ i _ _ | i == ident -> orig
    Def m i e1 e2 -> Def m i (rec e1) (rec e2)
    Assign (AIdent i) _ _ | i == ident -> orig
    Assign a e1 e2 -> Assign a (rec e1) (rec e2)
    ExprCell _ -> orig
    where rec e = subst e sub ident

instance (Display t) => Display (ExprPart t) where
  makeDoc a = case a of
    Var i -> text $ unIdent i
    Label n m e ->
      char '`' <> (text $ unLabelName n) <+> dispMod m <+> (parens $ makeDoc e)
    Onion e1 e2 -> makeDoc e1 <+> char '&' <+> makeDoc e2
    Func i e -> parens $
            text "fun" <+> (text $ unIdent i) <+> text "->" <+> makeDoc e
    Appl e1 e2 -> parens $ makeDoc e1 <+> makeDoc e2
    PrimInt i -> integer i
    PrimChar c -> quotes $ char c
    PrimUnit -> parens empty
    Case e brs -> parens $ text "case" <+> (parens $ makeDoc e) <+> text "of"
            <+> text "{" $+$
            (nest indentSize $ vcat $ punctuate semi $ map makeDoc brs)
            $+$ text "}"
    OnionSub e s -> makeDoc e <+> text "&-" <+> makeDoc s
    OnionProj e s -> makeDoc e <+> text "&." <+> makeDoc s
    EmptyOnion -> text "(&)"
    LazyOp op e1 e2 -> parens $ makeDoc e1 <+> makeDoc op <+> makeDoc e2
    EagerOp op e1 e2 -> parens $ makeDoc e1 <+> makeDoc op <+> makeDoc e2
    Def m i v e ->
      hsep [text "def", dispMod m, makeDoc i,
            text "=", makeDoc v, text "in", makeDoc e]
    Assign i v e -> hsep [makeDoc i, text "=", makeDoc v, text "in", makeDoc e]
    ExprCell c -> text "Cell #" <> int c
    where dispMod m = case m of
            Just Final -> text "final"
            Just Immutable -> text "immut"
            Nothing -> empty

-- TODO: fix parens
instance (Display t) => Display (Value t) where
  makeDoc x =
    case x of
      VLabel n v -> text "`" <> makeDoc n <+> parens (makeDoc v)
      VOnion v1 v2 -> parens (makeDoc v1) <+> text "&" <+> parens (makeDoc v2)
      VFunc i e -> text "fun" <+> text (unIdent i) <+> text "->" <+> makeDoc e
      VPrimInt i -> text $ show i
      VPrimChar c -> char c
      VPrimUnit -> text "()"
      VEmptyOnion -> text "(&)"

instance (Display t) => Display (Branch t) where
  makeDoc (Branch chi e) =
    makeDoc chi <+> text "->" <+> makeDoc e

instance Display (Chi a) where
  makeDoc chi =
    case chi of
      ChiTopVar x -> iDoc x
      ChiTopOnion p s -> makeDoc p <+> text "&" <+> makeDoc s
      ChiTopBind b -> makeDoc b
      ChiOnionMany p s -> makeDoc p <+> text "&" <+> makeDoc s
      ChiOnionOne p -> makeDoc p
      ChiBound i b -> iDoc i <> text ":" <> makeDoc b
      ChiUnbound p -> makeDoc p
      ChiPrim p -> makeDoc p
      ChiLabelShallow lbl x -> text "`" <> makeDoc lbl <+> iDoc x
      ChiLabelDeep lbl b -> text "`" <> makeDoc lbl <+> makeDoc b
      ChiFun -> text "fun"
      ChiInnerStruct s -> parens $ makeDoc s
    where iDoc = text . unIdent

instance Display Assignable where
  makeDoc (AIdent i) = makeDoc i
  makeDoc (ACell v) = makeDoc v

class Evaluated t a where
  value :: a -> Value t
  value v = fst $ vmPair v
  mapping :: a -> IntMap (Value t)
  mapping v = snd $ vmPair v

  vmPair :: a -> (Value t, IntMap (Value t))
  vmPair v = (value v, mapping v)

instance Evaluated t (Value t, IntMap (Value t)) where
  vmPair = id

instance Evaluated t (Value t) where
  value = id
  mapping = const IntMap.empty

