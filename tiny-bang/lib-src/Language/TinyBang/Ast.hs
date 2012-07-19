{-# LANGUAGE      FlexibleInstances
                , FlexibleContexts
                , EmptyDataDecls
                , GADTs
                , StandaloneDeriving
                , MultiParamTypeClasses
                , TypeSynonymInstances
                , UndecidableInstances
                , ScopedTypeVariables
                , TemplateHaskell
                , ImplicitParams
                #-}
module Language.TinyBang.Ast
( Expr
, ExprPart(..)
, Modifier(..)
, Value(..)
, Pattern(..)
, PrimaryPattern(..)
, Evaluated(..)
, CellId
, ePatVars
, exprVars
, VarsOp(..)
, exprFreeVars
, FreeVarsOp(..)
, subst
, SubstOp(..)
, PrecedenceOp(..)
, Precedence
-- Re-exported for convenience
, LazyOperator(..)
, EagerOperator(..)
, ProjTerm(..)
-- Smart constructors
, var
, label
, onion
, onionSub
, onionProj
, emptyOnion
, scape
, appl
, primInt
, primChar
, primUnit
, def
, assign
, lazyOp
, eagerOp
 ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM,liftM2,ap)
import Data.Monoid (Monoid, mempty, mappend)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
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
import Data.ExtensibleVariant
import Utils.Numeric
import Utils.Render.Display

-------------------------------------------------------------------------------

type CellId = Int

-- |Data type for representing TinyBang ASTs.
type Expr = Xv1 ExprPart

-- |Data type for representing TinyBang AST nodes.
data ExprPart t
  = Var Ident
  | Label LabelName (Maybe Modifier) t
  | Onion t t
  | OnionSub t ProjTerm
  | OnionProj t ProjTerm
  | EmptyOnion
  | Scape Pattern t
  | Appl t t
  | PrimInt Integer
  | PrimChar Char
  | PrimUnit
  | Def (Maybe Modifier) Ident t t
  | Assign Ident t t
  | LazyOp LazyOperator t t
  | EagerOp EagerOperator t t
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
  | VScape Pattern t
  | VPrimInt Integer
  | VPrimChar Char
  | VPrimUnit
  | VEmptyOnion
  deriving (Eq, Ord, Show)

data Pattern = Pattern Ident PrimaryPattern
  deriving (Eq, Ord, Show)

data PrimaryPattern
  = PatPrim T.PrimitiveType
  | PatLabel LabelName Ident PrimaryPattern
  | PatOnion [PrimaryPattern] -- An empty one of these matches anything.
  | PatFun
  deriving (Eq, Ord, Show)

-- Generate smart constructors for Expr type
$( genSmartConstr ''ExprPart )

-- |Obtains the set of bound variables in a pattern.
ePatVars :: Pattern -> Set Ident
ePatVars pat =
  case pat of
    Pattern i pp -> Set.singleton i `Set.union` rec pp
  where rec pat' =
          case pat' of
            PatPrim _ -> Set.empty
            PatLabel _ i pp -> rec pp `Set.union` Set.singleton i
            PatOnion pps -> Set.unions $ map rec pps
            PatFun -> Set.empty

-- TODO: a number of the following operations could be taking more advantage
-- of HomOp

-- |Obtains the set of free variables for an AST.
exprFreeVars :: (XvOp FreeVarsOp ast (Set Ident)) => ast -> Set Ident
exprFreeVars = xvOp FreeVarsOp
data FreeVarsOp = FreeVarsOp
instance (XvOp FreeVarsOp ast (Set Ident))
      => XvPart FreeVarsOp ExprPart ast (Set Ident) where
  xvPart FreeVarsOp part = case part of
    Var i -> Set.singleton i
    Scape pat e' -> exprFreeVars e' `Set.difference` ePatVars pat
    Def _ i e1 e2 ->
      (i `Set.delete` exprFreeVars e2) `Set.union` exprFreeVars e1
    Assign i e1 e2 -> i `Set.delete`
        (exprFreeVars e1 `Set.union` exprFreeVars e2)
    _ -> xvPart CatOp part (exprFreeVars :: ast -> Set Ident)

-- |Obtains the set of all variables in a given expression.  This includes the
--  variables found in patterns and other constructs.
exprVars :: (XvOp VarsOp ast (Set Ident)) => ast -> Set Ident
exprVars = xvOp VarsOp
data VarsOp = VarsOp
instance (XvOp VarsOp ast (Set Ident))
      => XvPart VarsOp ExprPart ast (Set Ident) where
  xvPart VarsOp part = case part of
    Var i -> Set.singleton i
    Scape pat e' -> ePatVars pat `Set.union` exprVars e'
    Def _ i e1 e2 -> (i `Set.insert` exprVars e1) `Set.union` exprVars e2
    Assign i e1 e2 -> i `Set.insert` (exprVars e1 `Set.union` exprVars e2)
    _ -> xvPart CatOp part (exprVars :: ast -> Set Ident)

-- |Performs a free variable substitution on the provided TinyBang AST.
subst :: (repl :<< ast
        , XvOp (SubstOp (repl ast)) ast ast)
      => ast            -- ^The AST
      -> repl ast       -- ^The substituting expression
      -> Ident          -- ^The identifier to substitute
      -> ast            -- ^The resulting AST
subst a r i = xvOp (SubstOp r i) a
data SubstOp repl = SubstOp repl Ident
instance (ExprPart :<< ast, repl :<< ast, XvOp (SubstOp (repl ast)) ast ast)
      => XvPart (SubstOp (repl ast)) ExprPart ast ast where
  xvPart (SubstOp sub ident) orig = case orig of
    Var i | i == ident -> inj $ sub
    Scape pat _ | ident `Set.member` ePatVars pat -> inj $ orig
    Def m i e1 e2 | i == ident -> inj $ Def m i (rec e1) e2
    Assign i e1 e2 | i == ident -> inj $ Assign i (rec e1) e2
    _ -> xvPart HomOp orig rec
    where rec e = subst e sub ident

-- |Specifies a homomorphic operation over TinyBang AST nodes.
instance (ExprPart :<< xv2, Monad m)
      => XvPart HomOpM ExprPart xv1 ((xv1 -> m xv2) -> m xv2) where
  xvPart HomOpM part f = liftM inj $ case part of
    Var i -> return $ Var i
    Label n m e -> Label n m <&> e
    Onion e1 e2 -> Onion <&> e1 <&*> e2
    OnionSub e s -> OnionSub <&> e <&^> s
    OnionProj e s -> OnionProj <&> e <&^> s
    Scape pat e -> Scape pat <&> e
    Appl e1 e2 -> Appl <&> e1 <&*> e2
    PrimInt v -> return $ PrimInt v
    PrimChar v -> return $ PrimChar v
    PrimUnit -> return $ PrimUnit
    EmptyOnion -> return $ EmptyOnion
    LazyOp op e1 e2 -> LazyOp op <&> e1 <&*> e2
    EagerOp op e1 e2 -> EagerOp op <&> e1 <&*> e2
    Def m i e1 e2 -> Def m i <&> e1 <&*> e2
    Assign i e1 e2 -> Assign i <&> e1 <&*> e2
    where (<&>) :: (xv2 -> b) -> xv1 -> m b
          c <&> e = liftM c $ f e
          infixl 4 <&>
          mc <&*> e = mc `ap` f e
          infixl 4 <&*>
          mc <&^> p = mc `ap` return p
          infixl 4 <&^>

-- |Specifies a catamorphic operation over TinyBang AST nodes.
instance (Monoid r, Monad m)
      => XvPart CatOpM ExprPart ast ((ast -> m r) -> m r) where
  xvPart CatOpM part f = case part of
    Var _ -> return $ mempty
    Label _ _ e -> f e
    Onion e1 e2 -> f e1 *+* f e2
    OnionSub e _ -> f e
    OnionProj e _ -> f e
    Scape _ e -> f e
    Appl e1 e2 -> f e1 *+* f e2
    PrimInt _ -> return $ mempty
    PrimChar _ -> return $ mempty
    PrimUnit -> return $ mempty
    EmptyOnion -> return $ mempty
    LazyOp _ e1 e2 -> f e1 *+* f e2
    EagerOp _ e1 e2 -> f e1 *+* f e2
    Def _ _ e1 e2 -> f e1 *+* f e2
    Assign _ e1 e2 -> f e1 *+* f e2
    where (*+*) :: m r -> m r -> m r
          x *+* y = liftM2 mappend x y
          infixl 4 *+*

-- |Describes an operation which produces, given an AST, its precedence in
--  terms of grouping.  Lower precedence operators have lower precedence values;
--  that is, + would have a lower number than *.  This is used during
--  pretty-printing to ensure that minimal parentheses are inserted.
data PrecedenceOp = PrecedenceOp
type Precedence = Double
instance XvPart PrecedenceOp ExprPart ast Precedence where
  xvPart PrecedenceOp part = case part of
    Var _ -> infinity
    Label _ _ _ -> 8
    Onion _ _ -> 4
    OnionSub _ _ -> 3
    OnionProj _ _ -> 3
    Scape _ _ -> 1
    Appl _ _ -> 7
    PrimInt _ -> infinity
    PrimChar _ -> infinity
    PrimUnit -> infinity
    EmptyOnion -> infinity
    LazyOp op _ _ ->
      case op of
        Plus -> 6
        Minus -> 6
    EagerOp op _ _ ->
      case op of
        Equal -> 5
        LessEqual -> 5
        GreaterEqual -> 5
    Def _ _ _ _ -> 1
    Assign _ _ _ -> 1

-- |Given a parent node and one of its children, this function will (1) convert
--  the child node into a document using makeDoc and (2) wrap that document in
--  parentheses if the child node has same or lower precedence than the parent
--  node.  The resulting document is returned.  This function is used in
--  defining the @Display@ instance for AST nodes.
childDoc :: (XvPart PrecedenceOp part ast Precedence
            ,XvOp PrecedenceOp ast Precedence
            ,Display (part ast), Display ast
            ,ConfigDisplayDebug c, ?conf::c)
         => part ast -> ast -> Doc
childDoc parent child =
  let doc = makeDoc child in
  if xvPart PrecedenceOp parent >= (xvOp PrecedenceOp child :: Precedence)
    then parens doc else doc
-- TODO: the above policy is a bit restrictive; it doesn't handle associativity
--       even on the same operator.  It'd be nice if "(a b) c" was printed as
--       "a b c".

-- |Specifies how to display TinyBang AST nodes.
instance (Display t, XvOp PrecedenceOp t Precedence)
      => Display (ExprPart t) where
  makeDoc a =
    let pDoc = childDoc a in
    case a of
      Var i -> text $ unIdent i
      Label n m e ->
        char '`' <> (text $ unLabelName n) <> dispMod m <+> pDoc e
      Onion e1 e2 -> nest indentSize $ pDoc e1 <+> char '&' </> pDoc e2
      Scape pat e -> nest indentSize $ makeDoc pat <+> text "->" </> pDoc e
      Appl e1 e2 -> nest indentSize $ pDoc e1 </> pDoc e2
      PrimInt i -> integer i
      PrimChar c -> squotes $ char c
      PrimUnit -> text "()"
      OnionSub e s -> pDoc e <+> text "&-" <+> makeDoc s
      OnionProj e s -> pDoc e <+> text "&." <+> makeDoc s
      EmptyOnion -> text "(&)"
      LazyOp op e1 e2 -> pDoc e1 <+> makeDoc op <+> pDoc e2
      EagerOp op e1 e2 -> pDoc e1 <+> makeDoc op <+> pDoc e2
      Def m i v e ->
        hsep [text "def", dispMod m, makeDoc i,
              text "=", pDoc v, text "in", pDoc e]
      Assign i v e -> hsep [makeDoc i, text "=", pDoc v, text "in", pDoc e]
      where dispMod m = case m of
              Just Final -> text " final"
              Just Immutable -> text " immut"
              Nothing -> empty

-- TODO: fix parens
instance (Display t) => Display (Value t) where
  makeDoc x =
    case x of
      VLabel n v -> text "`" <> makeDoc n <+> parens (makeDoc v)
      VOnion v1 v2 -> parens (makeDoc v1) <+> text "&" <+> parens (makeDoc v2)
      VScape pat e -> parens $ makeDoc pat <+> text "->" <+> makeDoc e
      VPrimInt i -> text $ show i
      VPrimChar c -> char c
      VPrimUnit -> text "()"
      VEmptyOnion -> text "(&)"

simplifyPrimaryPattern :: PrimaryPattern -> Maybe PrimaryPattern
simplifyPrimaryPattern pp =
  case pp of
    PatPrim _ -> Just pp
    PatLabel lbl i pp' ->
      Just $ PatLabel lbl i $
        maybe (PatOnion []) id $ simplifyPrimaryPattern pp'
    PatOnion pps ->
      let pps' = catMaybes $ map simplifyPrimaryPattern pps in
      case pps' of
        [] -> Nothing
        [spp] -> Just spp
        _ -> Just $ PatOnion pps'
    PatFun -> Just pp
    
ppMakeDoc :: (ConfigDisplayDebug b, ?conf :: b)
          => PrimaryPattern -> Maybe Doc
ppMakeDoc ppat = makeDoc <$> simplifyPrimaryPattern ppat

instance Display Pattern where
  makeDoc pat =
    case pat of
      Pattern i pp ->
        let iDoc = makeDoc i
            mppDoc = ppMakeDoc pp in
        case (unIdent i == "_", mppDoc) of
          (True, Nothing) -> makeDoc $ PatOnion []
          (True, Just ppDoc) -> ppDoc
          (False, Nothing) -> iDoc
          (False, Just ppDoc) -> iDoc <> colon <> ppDoc

instance Display PrimaryPattern where
  makeDoc pat =
    let pat' = maybe (PatOnion []) id $ simplifyPrimaryPattern pat in
    case pat' of
      PatPrim tprim -> makeDoc tprim
      PatLabel lbl i pp ->
        let ppDoc = maybe empty (colon <>) $ ppMakeDoc pp in
        text "`" <> makeDoc lbl <+> makeDoc i <> ppDoc
      PatOnion pps ->
        let ppDocs = catMaybes $ map ppMakeDoc pps in
        if null ppDocs
          then text "any"
          else parens $ hang indentSize $ fillSep $
                intersperse (char '&') $ ppDocs
      PatFun -> text "fun"

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
