{-# LANGUAGE  FlexibleInstances
            , FlexibleContexts
            , EmptyDataDecls
            , GADTs
            , StandaloneDeriving
            , MultiParamTypeClasses
            , ScopedTypeVariables
            , TemplateHaskell
            , TypeSynonymInstances
            #-}
module Language.LittleBang.Ast
( Expr
, ExprPart(..)
-- Re-exported for convenience
, TA.Pattern(..)
, TA.Modifier(..)
, TA.ProjTerm(..)
, TA.exprFreeVars
, TA.exprVars
-- Smart constructors
, self
, prior
, proj
, projAssign
, onion
, scape
, appl
) where

import Control.Monad (liftM, liftM2, ap)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.ExtensibleVariant
import qualified Language.TinyBang.Ast as TA
import Language.TinyBang.Ast (exprFreeVars, exprVars)
import Language.TinyBang.Types.UtilTypes
import Utils.Numeric
import Utils.Render.Display

-------------------------------------------------------------------------------

-- |Data type for representing LittleBang AST nodes.
data ExprPart t
  = Self
  | Prior
  | Proj t Ident
  | ProjAssign t Ident t t
  | Onion t t              -- LittleBang onions include semantics for prior
  | Scape TA.Pattern t     -- LittleBang scapes include a variable for self
  | Appl t t               -- LittleBang function application binds self
  deriving (Eq, Ord, Show)

$( genSmartConstr ''ExprPart )

-- |Data type for a LittleBang AST.
type Expr = Xv2 TA.ExprPart ExprPart

-- |Obtains the set of free variables for LittleBang AST nodes.
instance (XvOp TA.FreeVarsOp ast (Set Ident))
      => XvPart TA.FreeVarsOp ExprPart ast (Set Ident) where
  xvPart TA.FreeVarsOp ast = case ast of
    Self -> Set.empty
    Prior -> Set.empty
    Proj e _ -> exprFreeVars e
    ProjAssign e1 _ e2 e3 -> Set.unions $ map exprFreeVars [e1,e2,e3]
    Onion e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2
    Scape p e -> exprFreeVars e `Set.difference` TA.ePatVars p
    Appl e1 e2 -> exprFreeVars e1 `Set.union` exprFreeVars e2

-- |Obtains the set of variables for LittleBang AST nodes.
instance (XvOp TA.VarsOp ast (Set Ident))
      => XvPart TA.VarsOp ExprPart ast (Set Ident) where
  xvPart TA.VarsOp ast = case ast of
    Self -> Set.empty
    Prior -> Set.empty
    Proj e _ -> exprVars e
    ProjAssign e1 _ e2 e3 -> Set.unions $ map exprVars [e1,e2,e3]
    Onion e1 e2 -> exprVars e1 `Set.union` exprVars e2
    Scape p e -> exprVars e `Set.union` TA.ePatVars p
    Appl e1 e2 -> exprVars e1 `Set.union` exprVars e2

-- |Defines a homomorphism over a tree containing LittleBang AST nodes.
instance (ExprPart :<< xv2, Monad m)
      => XvPart HomOpM ExprPart xv1 ((xv1 -> m xv2) -> m xv2) where
  xvPart HomOpM part f = liftM inj $ case part of
    Self -> return $ Self
    Prior -> return $ Prior
    Proj e i -> Proj <&> e <&^> i
    ProjAssign e1 i e2 e3 -> ProjAssign <&> e1 <&^> i <&*> e2 <&*> e3
    Onion e1 e2 -> Onion <&> e1 <&*> e2
    Scape p e -> Scape p <&> e
    Appl e1 e2 -> Appl <&> e1 <&*> e2
    where (<&>) :: (xv2 -> b) -> xv1 -> m b
          c <&> e = liftM c $ f e
          infixl 4 <&>
          (<&*>) :: m (xv2 -> b) -> xv1 -> m b
          mc <&*> e = mc `ap` f e
          infixl 4 <&*>
          (<&^>) :: m (a -> b) -> a -> m b
          mc <&^> p = mc `ap` return p
          infixl 4 <&^>

-- |Defines a catamorphism over a tree containing LittleBang AST nodes.
instance (Monoid r, Monad m)
      => XvPart CatOpM ExprPart xv1 ((xv1 -> m r) -> m r) where
  xvPart CatOpM part f = case part of
    Self -> return $ mempty
    Prior -> return $ mempty
    Proj e _ -> f e
    ProjAssign e1 _ e2 e3 -> f e1 *+* f e2 *+* f e3
    Onion e1 e2 -> f e1 *+* f e2
    Scape _ e -> f e
    Appl e1 e2 -> f e1 *+* f e2
    where x *+* y = (liftM2 mappend) x y
          infixl 4 *+*

-- |Defines precedence of LittleBang AST nodes.
instance XvPart TA.PrecedenceOp ExprPart ast TA.Precedence where
  xvPart TA.PrecedenceOp part = case part of
    Self -> infinity
    Prior -> infinity
    Proj _ _ -> 9
    ProjAssign _ _ _ _ -> 1
    Onion _ _ -> 4
    Scape _ _ -> 1
    Appl _ _ -> 7

-- |Displays LittleBang AST nodes.
instance (Display t) => Display (ExprPart t) where
  makeDoc a = case a of
    Self -> text "self"
    Prior -> text "prior"
    Proj e i -> makeDoc e <> char '.' <> makeDoc i
    ProjAssign e1 i e2 e3 -> makeDoc e1 <> char '.' <> makeDoc i <+>
                             text "=" <+> makeDoc e2 <+> text "in" <+>
                             makeDoc e3
    Onion e1 e2 -> makeDoc e1 <+> text "&" <+> makeDoc e2
    Scape p e -> makeDoc p <+> text "->" <+> makeDoc e
    Appl e1 e2 -> makeDoc e1 <+> makeDoc e2

