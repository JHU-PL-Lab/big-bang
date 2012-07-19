{-# LANGUAGE  FlexibleContexts
            , FlexibleInstances
            , MultiParamTypeClasses
            , UndecidableInstances
            , ScopedTypeVariables
            , TypeSynonymInstances
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

import Data.ExtensibleVariant
import Utils.Numeric
import Utils.Render.Display

type CellId = Int

type Expr = Xv2 A.ExprPart ExprPart

-- |Data type for representing TinyBang intermediate AST nodes.
data ExprPart t
    = ExprCell CellId
    | AssignCell CellId t t
    deriving (Eq, Ord, Show)

-- |Provides behavior for free variable searches.
instance (XvOp A.FreeVarsOp ast (Set U.Ident))
      => XvPart A.FreeVarsOp ExprPart ast (Set U.Ident) where
  xvPart A.FreeVarsOp part =
        xvPart CatOp part (A.exprFreeVars :: ast -> Set U.Ident)

-- |Provides behavior for variable searches.
instance (XvOp A.VarsOp ast (Set U.Ident))
      => XvPart A.VarsOp ExprPart ast (Set U.Ident) where
  xvPart A.VarsOp part =
        xvPart CatOp part (A.exprVars :: ast -> Set U.Ident)

-- |Provides behavior for free variable substitution.
instance (ExprPart :<< ast
        , A.ExprPart :<< ast
        , repl :<< ast
        , XvOp (A.SubstOp (repl ast)) ast ast)
      => XvPart (A.SubstOp (repl ast)) ExprPart ast ast where
  xvPart (A.SubstOp sub ident) orig = xvPart HomOp orig rec
    where rec e = A.subst e sub ident

-- |Performs a free variable cell substitution on the provided TinyBang AST.
--  This routine will address both LHS and RHS variables.
substCell :: (A.ExprPart :<< ast
            , ExprPart :<< ast
            , XvPart HomOp ExprPart ast ((ast -> ast) -> ast)
            , XvOp SubstCellOp ast (CellId -> U.Ident -> ast))
          => ast -> CellId -> U.Ident -> ast
substCell = xvOp SubstCellOp
data SubstCellOp = SubstCellOp
instance (ExprPart :<< ast
        , A.ExprPart :<< ast
        , XvPart HomOp A.ExprPart ast ((ast -> ast) -> ast)
        --, XvPart SubstOp A.ExprPart ast (A.ExprPart ast -> Ident -> ast)
        , XvOp SubstCellOp ast (CellId -> U.Ident -> ast))
      => XvPart SubstCellOp A.ExprPart ast (CellId -> U.Ident -> ast) where
  xvPart SubstCellOp orig cell ident = case orig of
    A.Var i | i == ident -> inj $ ExprCell cell
    A.Def m i e1 e2 | i == ident -> inj $ A.Def m i (rec e1) e2
    A.Assign i e1 e2 | i == ident ->
        inj $ AssignCell cell (rec e1) (rec e2)
    A.Scape chi _ | ident `Set.member` A.ePatVars chi -> inj $ orig
--    A.Case e brs -> inj $ A.Case (rec e) $
--        map (\(A.Branch pat bre) -> A.Branch pat $
--          (if ident `Set.member` A.ePatVars pat then id else rec) bre) brs
    _ -> xvPart HomOp orig rec
    where rec :: ast -> ast
          rec e = substCell e cell ident

-- |Performs a free variable cell substitution on the provided TinyBang AST
--  containing intermediate nodes.
instance (XvOp SubstCellOp ast (CellId -> U.Ident -> ast)
        , A.ExprPart :<< ast
        , ExprPart :<< ast)
      => XvPart SubstCellOp ExprPart ast (CellId -> U.Ident -> ast) where
  xvPart SubstCellOp orig cell ident = case orig of
    ExprCell _ -> inj $ orig
    AssignCell c e1 e2 -> inj $ AssignCell c (rec e1) (rec e2)
    where rec e = substCell e cell ident

-- |Specifies a homomorphic operation over TinyBang intermediate AST nodes.
instance (ExprPart :<< xv2, Monad m)
      => XvPart HomOpM ExprPart xv1 ((xv1 -> m xv2) -> m xv2) where
  xvPart HomOpM part f = liftM inj $ case part of
    ExprCell c -> return $ ExprCell c
    AssignCell c e1 e2 -> liftM2 (AssignCell c) (f e1) (f e2)

-- |Specifies a catamorphic operation over TinyBang intermediate AST nodes.
instance (Monoid r, Monad m)
      => XvPart CatOpM ExprPart ast ((ast -> m r) -> m r) where
  xvPart CatOpM part f = case part of
    ExprCell _ -> return $ mempty
    AssignCell _ e1 e2 -> liftM2 mappend (f e1) (f e2)

-- |Specifies the precedence for TinyBang interpreter nodes.
instance XvPart A.PrecedenceOp ExprPart t A.Precedence where
  xvPart A.PrecedenceOp part = case part of
    ExprCell _ -> -infinity
    AssignCell _ _ _ -> 1

-- |Specifies how to display TinyBang interpreter AST nodes.
instance (Display t) => Display (ExprPart t) where
  makeDoc a = case a of
    ExprCell c -> text "Cell #" <> int c
    AssignCell c e1 e2 -> text "Cell #" <> int c <+> text "=" <+>
                          makeDoc e1 <+> text "in" <+> makeDoc e2

