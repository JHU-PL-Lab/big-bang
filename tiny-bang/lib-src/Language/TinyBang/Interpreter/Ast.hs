{-# LANGUAGE  FlexibleContexts
            , FlexibleInstances
            , MultiParamTypeClasses
            , UndecidableInstances
            , ScopedTypeVariables
            #-}

{-|
  A module containing a language extension for TinyBang including cells.  The
  primary purpose of this extension is to separate intermediate state-related
  constructs from the rest of the TinyBang grammar.
-}

module Language.TinyBang.Interpreter.Ast
( CellId
, Expr
, ExprPart(..)
, SubstCellOp
, substCell
) where

import Control.Monad (liftM, liftM2)
import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Types.UtilTypes as U

import Utils.Language.Ast
import Utils.Render.Display

type CellId = Int

type Expr = Ast2 A.ExprPart ExprPart

-- |Data type for representing TinyBang intermediate AST nodes.
data ExprPart t
    = ExprCell CellId
    | AssignCell CellId t t
    deriving (Eq, Ord, Show)

-- |Provides behavior for free variable searches.
instance (AstOp A.FreeVarsOp ast (Set U.Ident))
      => AstStep A.FreeVarsOp ExprPart ast (Set U.Ident) where
  aststep A.FreeVarsOp part =
        aststep CatOp part (A.exprFreeVars :: ast -> Set U.Ident)

-- |Provides behavior for variable searches.
instance (AstOp A.VarsOp ast (Set U.Ident))
      => AstStep A.VarsOp ExprPart ast (Set U.Ident) where
  aststep A.VarsOp part =
        aststep CatOp part (A.exprVars :: ast -> Set U.Ident)

-- |Provides behavior for free variable substitution.
instance (AstWrap ExprPart ast
        , AstWrap A.ExprPart ast
        , AstOp A.SubstOp ast (A.ExprPart ast -> U.Ident -> ast))
      => AstStep A.SubstOp ExprPart ast (A.ExprPart ast -> U.Ident -> ast) where
  aststep A.SubstOp orig sub ident = aststep HomOp orig rec
    where rec e = A.subst e sub ident

-- |Performs a free variable cell substitution on the provided TinyBang AST.
--  This routine will address both LHS and RHS variables.
substCell :: (AstWrap A.ExprPart ast
            , AstWrap ExprPart ast
            , AstStep HomOp ExprPart ast ((ast -> ast) -> ast)
            , AstOp SubstCellOp ast (CellId -> U.Ident -> ast))
          => ast -> CellId -> U.Ident -> ast
substCell = astop SubstCellOp
data SubstCellOp = SubstCellOp
instance (AstWrap ExprPart ast
        , AstWrap A.ExprPart ast
        , AstStep HomOp A.ExprPart ast ((ast -> ast) -> ast)
        --, AstStep SubstOp A.ExprPart ast (A.ExprPart ast -> Ident -> ast)
        , AstOp SubstCellOp ast (CellId -> U.Ident -> ast))
      => AstStep SubstCellOp A.ExprPart ast (CellId -> U.Ident -> ast) where
  aststep SubstCellOp orig cell ident = case orig of
    A.Var i | i == ident -> astwrap $ ExprCell cell
    A.Def m i e1 e2 | i == ident -> astwrap $ A.Def m i (rec e1) e2
    A.Assign i e1 e2 | i == ident ->
        astwrap $ AssignCell cell (rec e1) (rec e2)
    A.Scape chi _ | ident `Set.member` A.ePatVars chi -> astwrap $ orig
--    A.Case e brs -> astwrap $ A.Case (rec e) $
--        map (\(A.Branch pat bre) -> A.Branch pat $
--          (if ident `Set.member` A.ePatVars pat then id else rec) bre) brs
    _ -> aststep HomOp orig rec
    where rec :: ast -> ast
          rec e = substCell e cell ident

-- |Performs a free variable cell substitution on the provided TinyBang AST
--  containing intermediate nodes.
instance (AstOp SubstCellOp ast (CellId -> U.Ident -> ast)
        , AstWrap A.ExprPart ast
        , AstWrap ExprPart ast)
      => AstStep SubstCellOp ExprPart ast (CellId -> U.Ident -> ast) where
  aststep SubstCellOp orig cell ident = case orig of
    ExprCell _ -> astwrap $ orig
    AssignCell c e1 e2 -> astwrap $ AssignCell c (rec e1) (rec e2)
    where rec e = substCell e cell ident

-- |Specifies a homomorphic operation over TinyBang intermediate AST nodes.
instance (AstWrap ExprPart ast2
        , Monad m)
      => AstStep HomOpM ExprPart ast1 ((ast1 -> m ast2) -> m ast2) where
  aststep HomOpM part f = liftM astwrap $ case part of
    ExprCell c -> return $ ExprCell c
    AssignCell c e1 e2 -> liftM2 (AssignCell c) (f e1) (f e2)

-- |Specifies a catamorphic operation over TinyBang intermediate AST nodes.
instance (Monoid r, Monad m)
      => AstStep CatOpM ExprPart ast ((ast -> m r) -> m r) where
  aststep CatOpM part f = case part of
    ExprCell _ -> return $ mempty
    AssignCell _ e1 e2 -> liftM2 mappend (f e1) (f e2)

-- |Specifies how to display TinyBang interpreter AST nodes.
instance (Display t) => Display (ExprPart t) where
  makeDoc a = case a of
    ExprCell c -> text "Cell #" <> int c
    AssignCell c e1 e2 -> text "Cell #" <> int c <+> text "=" <+>
                          makeDoc e1 <+> text "in" <+> makeDoc e2
