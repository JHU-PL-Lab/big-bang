{-# LANGUAGE      FlexibleInstances
                , FlexibleContexts
                , EmptyDataDecls
                , GADTs
                , StandaloneDeriving
                , MultiParamTypeClasses
                , TypeSynonymInstances
                , UndecidableInstances
                , ScopedTypeVariables
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
-- Re-exported for convenience
, LazyOperator(..)
, EagerOperator(..)
, ProjTerm(..)
, Assignable(..)
) where

import Control.Monad (liftM,liftM2,ap)
import Data.Monoid (Monoid, mempty, mappend)
import Data.List (intersperse)
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
  | Scape Pattern t
--  | Func Ident t
  | Appl t t
  | PrimInt Integer
  | PrimChar Char
  | PrimUnit
--  | Case t (Branches t)
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
--  | VFunc Ident t
  | VPrimInt Integer
  | VPrimChar Char
  | VPrimUnit
  | VEmptyOnion
  deriving (Eq, Ord, Show)

data Assignable = ACell CellId | AIdent Ident
  deriving (Eq, Ord, Show)

data Pattern = Pattern Ident PrimaryPattern
  deriving (Eq, Ord, Show)

data PrimaryPattern
  = PatPrim T.PrimitiveType
  | PatLabel LabelName Ident PrimaryPattern
  | PatOnion [PrimaryPattern] -- An empty one of these matches anything.
  | PatFun
  deriving (Eq, Ord, Show)

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
exprFreeVars :: (AstOp FreeVarsOp ast (Set Ident)) => ast -> Set Ident
exprFreeVars = astop FreeVarsOp
data FreeVarsOp = FreeVarsOp
instance (AstOp FreeVarsOp ast (Set Ident))
      => AstStep FreeVarsOp ExprPart ast (Set Ident) where
  aststep FreeVarsOp part = case part of
    Var i -> Set.singleton i
    Scape pat e' -> exprFreeVars e' `Set.difference` ePatVars pat
--    Func i e' -> i `Set.delete` exprFreeVars e'
--    Case e' brs -> Set.union (exprFreeVars e') $ Set.unions $
--      map (\(Branch chi e'') ->
--              exprFreeVars e'' `Set.difference` ePatVars chi) brs
    Def _ i e1 e2 ->
      (i `Set.delete` exprFreeVars e2) `Set.union` exprFreeVars e1
    Assign i e1 e2 -> i `Set.delete`
        (exprFreeVars e1 `Set.union` exprFreeVars e2)
    _ -> aststep CatOp part (exprFreeVars :: ast -> Set Ident)

-- |Obtains the set of all variables in a given expression.  This includes the
--  variables found in patterns and other constructs.
exprVars :: (AstOp VarsOp ast (Set Ident)) => ast -> Set Ident
exprVars = astop VarsOp
data VarsOp = VarsOp
instance (AstOp VarsOp ast (Set Ident))
      => AstStep VarsOp ExprPart ast (Set Ident) where
  aststep VarsOp part = case part of
    Var i -> Set.singleton i
    Scape pat e' -> ePatVars pat `Set.union` exprVars e'
--    Func i e' -> i `Set.insert` exprVars e'
--    Case e' brs -> Set.union (exprVars e') $ Set.unions $
--      map (\(Branch chi e'') ->
--              exprVars e'' `Set.difference` ePatVars chi) brs
    Def _ i e1 e2 -> (i `Set.insert` exprVars e1) `Set.union` exprVars e2
    Assign i e1 e2 -> i `Set.insert` (exprVars e1 `Set.union` exprVars e2)
    _ -> aststep CatOp part (exprVars :: ast -> Set Ident)

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
  aststep SubstOp orig sub ident = case orig of
    Var i | i == ident -> astwrap $ sub
    Scape pat _ | ident `Set.member` ePatVars pat -> astwrap $ orig
--    Func i _ | i == ident -> astwrap $ orig
--    Case e branches -> astwrap $ Case (rec e) $
--        map (\(Branch pat bre) -> Branch pat $
--          (if ident `Set.member` ePatVars pat then id else rec) bre) branches
    Def m i e1 e2 | i == ident -> astwrap $ Def m i (rec e1) e2
    Assign i e1 e2 | i == ident -> astwrap $ Assign i (rec e1) e2
    _ -> aststep HomOp orig rec
    where rec e = subst e sub ident

-- |Specifies a homomorphic operation over TinyBang AST nodes.
instance (AstWrap ExprPart ast2
         ,Monad m)
      => AstStep HomOpM ExprPart ast1 ((ast1 -> m ast2) -> m ast2) where
  aststep HomOpM part f = liftM astwrap $ case part of
    Var i -> return $ Var i
    Label n m e -> Label n m <&> e
    Onion e1 e2 -> Onion <&> e1 <&*> e2
    OnionSub e s -> OnionSub <&> e <&^> s
    OnionProj e s -> OnionProj <&> e <&^> s
    Scape pat e -> Scape pat <&> e
--    Func i e -> Func i <&> e
    Appl e1 e2 -> Appl <&> e1 <&*> e2
    PrimInt v -> return $ PrimInt v
    PrimChar v -> return $ PrimChar v
    PrimUnit -> return $ PrimUnit
--    Case e brs -> do
--        e' <- f e
--        brs' <- mapM appbr brs
--        return $ Case e' brs'
    EmptyOnion -> return $ EmptyOnion
    LazyOp op e1 e2 -> LazyOp op <&> e1 <&*> e2
    EagerOp op e1 e2 -> EagerOp op <&> e1 <&*> e2
    Def m i e1 e2 -> Def m i <&> e1 <&*> e2
    Assign i e1 e2 -> Assign i <&> e1 <&*> e2
    where (<&>) :: (ast2 -> b) -> ast1 -> m b
          c <&> e = liftM c $ f e
          infixl 4 <&>
          mc <&*> e = mc `ap` f e
          infixl 4 <&*>
          mc <&^> p = mc `ap` return p
          infixl 4 <&^>
--          appbr (Branch pat bre) = do
--            bre' <- f bre
--            return $ Branch pat bre'

-- |Specifies a catamorphic operation over TinyBang AST nodes.
instance (Monoid r, Monad m)
      => AstStep CatOpM ExprPart ast ((ast -> m r) -> m r) where
  aststep CatOpM part f = case part of
    Var _ -> return $ mempty
    Label _ _ e -> f e
    Onion e1 e2 -> f e1 *+* f e2
    OnionSub e _ -> f e
    OnionProj e _ -> f e
    Scape _ e -> f e
--    Func _ e -> f e
    Appl e1 e2 -> f e1 *+* f e2
    PrimInt _ -> return $ mempty
    PrimChar _ -> return $ mempty
    PrimUnit -> return $ mempty
--    Case e brs -> foldl (*+*) (f e) $ map (\(Branch _ e') -> f e') brs
    EmptyOnion -> return $ mempty
    LazyOp _ e1 e2 -> f e1 *+* f e2
    EagerOp _ e1 e2 -> f e1 *+* f e2
    Def _ _ e1 e2 -> f e1 *+* f e2
    Assign _ e1 e2 -> f e1 *+* f e2
    where (*+*) :: m r -> m r -> m r
          x *+* y = liftM2 mappend x y
          infixl 4 *+*

-- |Specifies how to display TinyBang AST nodes.
instance (Display t) => Display (ExprPart t) where
  makeDoc a = case a of
    Var i -> text $ unIdent i
    Label n m e ->
      char '`' <> (text $ unLabelName n) <+> dispMod m <+> (parens $ makeDoc e)
    Onion e1 e2 -> makeDoc e1 <+> char '&' <+> makeDoc e2
    Scape pat e -> parens $
      makeDoc pat <+> text "->" <+> makeDoc e
--    Func i e -> parens $
--            text "fun" <+> (text $ unIdent i) <+> text "->" <+> makeDoc e
    Appl e1 e2 -> parens $ makeDoc e1 <+> makeDoc e2
    PrimInt i -> integer i
    PrimChar c -> quotes $ char c
    PrimUnit -> parens empty
--    Case e brs -> parens $ text "case" <+> (parens $ makeDoc e) <+> text "of"
--            <+> text "{" $+$
--            (nest indentSize $ vcat $ punctuate semi $ map makeDoc brs)
--            $+$ text "}"
    OnionSub e s -> makeDoc e <+> text "&-" <+> makeDoc s
    OnionProj e s -> makeDoc e <+> text "&." <+> makeDoc s
    EmptyOnion -> text "(&)"
    LazyOp op e1 e2 -> parens $ makeDoc e1 <+> makeDoc op <+> makeDoc e2
    EagerOp op e1 e2 -> parens $ makeDoc e1 <+> makeDoc op <+> makeDoc e2
    Def m i v e ->
      hsep [text "def", dispMod m, makeDoc i,
            text "=", makeDoc v, text "in", makeDoc e]
    Assign i v e -> hsep [makeDoc i, text "=", makeDoc v, text "in", makeDoc e]
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
      VScape pat e -> parens $ makeDoc pat <+> text "->" <+> makeDoc e
--      VFunc i e -> text "fun" <+> text (unIdent i) <+> text "->" <+> makeDoc e
      VPrimInt i -> text $ show i
      VPrimChar c -> char c
      VPrimUnit -> text "()"
      VEmptyOnion -> text "(&)"

instance Display Pattern where
  makeDoc pat =
    case pat of
      Pattern i pp -> makeDoc i <> colon <> makeDoc pp

instance Display PrimaryPattern where
  makeDoc pat =
    case pat of
      PatPrim tprim -> makeDoc tprim
      PatLabel lbl i pp -> makeDoc lbl <+> makeDoc i <> colon <> makeDoc pp
      PatOnion pps -> parens $ sep $ intersperse (char '&') $ map makeDoc pps
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
