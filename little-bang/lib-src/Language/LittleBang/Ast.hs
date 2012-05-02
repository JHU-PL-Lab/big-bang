{-# LANGUAGE  FlexibleInstances
            , FlexibleContexts
            , EmptyDataDecls
            , GADTs
            , StandaloneDeriving
            , MultiParamTypeClasses
            #-}
module Language.LittleBang.Ast
( Expr
, ExprPart(..)
-- Re-exported for convenience
, TA.Chi(..)
, TA.ChiMain
, TA.ChiStruct
, TA.ChiBind
, TA.ChiPrimary
, TA.Branches
, TA.Branch(..)
, TA.Modifier(..)
, TA.ProjTerm(..)
, TA.exprFreeVars
, TA.exprVars
) where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Language.TinyBang.Ast as TA
import Language.TinyBang.Ast (exprFreeVars, exprVars)
import Language.TinyBang.Types.UtilTypes
import Utils.Language.Ast
import Utils.Render.Display

-------------------------------------------------------------------------------

-- |Data type for representing LittleBang AST nodes.
data ExprPart t
  = Self
  | Prior
  | Proj t Ident
  | ProjAssign t Ident t t
  | Case t (TA.Branches t) -- LittleBang cases include semantics for self
  | Onion t t              -- LittleBang onions include semantics for prior
  | Func Ident t           -- LittleBang functions include a variable for self
  | Appl t t               -- LittleBang function application applies (&)
  deriving (Eq, Ord, Show)

-- |Data type for a LittleBang AST.
type Expr = Ast2 TA.ExprPart ExprPart

-- |Obtains the set of free variables for LittleBang AST nodes.
instance (AstOp TA.FreeVarsOp ast (Set Ident))
      => AstStep TA.FreeVarsOp ExprPart ast (Set Ident) where
  aststep TA.FreeVarsOp ast = case ast of
    Self -> Set.empty
    Prior -> Set.empty
    Proj e _ -> exprFreeVars e
    ProjAssign e1 _ e2 e3 -> Set.unions $ map exprFreeVars [e1,e2,e3]
    Case e branches -> Set.union (exprFreeVars e) $ Set.unions $
        map (\(TA.Branch pat patexp) ->
            exprFreeVars patexp `Set.difference` TA.ePatVars pat) branches
    Onion e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2
    Func i e -> i `Set.delete` exprFreeVars e
    Appl e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2

-- |Obtains the set of variables for LittleBang AST nodes.
instance (AstOp TA.VarsOp ast (Set Ident))
      => AstStep TA.VarsOp ExprPart ast (Set Ident) where
  aststep TA.VarsOp ast = case ast of
    Self -> Set.empty
    Prior -> Set.empty
    Proj e _ -> exprVars e
    ProjAssign e1 _ e2 e3 -> Set.unions $ map exprVars [e1,e2,e3]
    Case e branches -> Set.union (exprVars e) $ Set.unions $
        map (\(TA.Branch pat patexp) ->
            exprVars patexp `Set.union` TA.ePatVars pat) branches
    Onion e1 e2 -> exprVars e1 `Set.union` exprVars e2
    Func i e -> i `Set.insert` exprVars e
    Appl e1 e2 -> exprVars e1 `Set.union` exprVars e2

-- |Defines a homomorphism over a tree containing LittleBang AST nodes.
instance (AstWrap ExprPart ast2
         ,Monad m)
      => AstStep HomOpM ExprPart ast1 ((ast1 -> m ast2) -> m ast2) where
  aststep HomOpM ast = \f -> case ast of
    Self -> return $ astwrap $ Self
    Prior -> return $ astwrap $ Prior
    Proj e i -> do
        e' <- f e
        return $ astwrap $ Proj e' i
    ProjAssign e1 i e2 e3 -> do
        e1' <- f e1
        e2' <- f e2
        e3' <- f e3
        return $ astwrap $ ProjAssign e1' i e2' e3'
    Case e brs -> do
        e' <- f e
        brs' <- mapM (appbrs f) brs
        return $ astwrap $ Case e' brs'
    Onion e1 e2 -> do
        e1' <- f e1
        e2' <- f e2
        return $ astwrap $ Onion e1' e2'
    Func i e -> do
        e' <- f e
        return $ astwrap $ Func i e'
    Appl e1 e2 -> do
        e1' <- f e1
        e2' <- f e2
        return $ astwrap $ Appl e1' e2'
    where appbrs f (TA.Branch pat expr) = do
            expr' <- f expr
            return $ TA.Branch pat expr'

-- |Displays LittleBang AST nodes.
instance (Display t) => Display (ExprPart t) where
  makeDoc a = case a of
    Self -> text "self"
    Prior -> text "prior"
    Proj e i -> makeDoc e <> char '.' <> makeDoc i
    ProjAssign e1 i e2 e3 -> makeDoc e1 <> char '.' <> makeDoc i <+>
                             text "=" <+> makeDoc e2 <+> text "in" <+>
                             makeDoc e3
    Case e branches -> text "case" <+> (parens $ makeDoc e) <+> text "of" <+>
        text "{" <+>
            (nest indentSize $ vcat $ punctuate semi $ map makeDoc branches)
        <+> text "}"
    Onion e1 e2 -> makeDoc e1 <+> text "&" <+> makeDoc e2
    Func i e -> text "fun" <+> makeDoc i <+> text "->" <+> makeDoc e
    Appl e1 e2 -> makeDoc e1 <+> makeDoc e2

