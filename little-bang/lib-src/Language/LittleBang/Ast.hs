{-# LANGUAGE  FlexibleInstances
            , FlexibleContexts
            , EmptyDataDecls
            , GADTs
            , StandaloneDeriving
            , MultiParamTypeClasses
            , ScopedTypeVariables
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

import Control.Monad (liftM, liftM2, ap)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Language.TinyBang.Ast as TA
import Language.TinyBang.Ast (exprFreeVars, exprVars)
import Language.TinyBang.Types.UtilTypes
import Data.ExtensibleVariant
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
instance (XvOp TA.FreeVarsOp ast (Set Ident))
      => XvPart TA.FreeVarsOp ExprPart ast (Set Ident) where
  xvpart TA.FreeVarsOp ast = case ast of
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
instance (XvOp TA.VarsOp ast (Set Ident))
      => XvPart TA.VarsOp ExprPart ast (Set Ident) where
  xvpart TA.VarsOp ast = case ast of
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
instance ((:<<) ExprPart ast2
         ,Monad m)
      => XvPart HomOpM ExprPart ast1 ((ast1 -> m ast2) -> m ast2) where
  xvpart HomOpM part f = liftM inj $ case part of
    Self -> return $ Self
    Prior -> return $ Prior
    Proj e i -> Proj <&> e <&^> i
    ProjAssign e1 i e2 e3 -> ProjAssign <&> e1 <&^> i <&*> e2 <&*> e3
    Case e brs -> do
        e' <- f e
        brs' <- mapM appbr brs
        return $ Case e' brs'
    Onion e1 e2 -> Onion <&> e1 <&*> e2
    Func i e -> Func i <&> e
    Appl e1 e2 -> Appl <&> e1 <&*> e2
    where (<&>) :: (ast2 -> b) -> ast1 -> m b
          c <&> e = liftM c $ f e
          infixl 4 <&>
          (<&*>) :: m (ast2 -> b) -> ast1 -> m b
          mc <&*> e = mc `ap` f e
          infixl 4 <&*>
          (<&^>) :: m (a -> b) -> a -> m b
          mc <&^> p = mc `ap` return p
          infixl 4 <&^>
          appbr (TA.Branch pat expr) = do
            expr' <- f expr
            return $ TA.Branch pat expr'

-- |Defines a catamorphism over a tree containing LittleBang AST nodes.
instance (Monoid r, Monad m)
      => XvPart CatOpM ExprPart ast1 ((ast1 -> m r) -> m r) where
  xvpart CatOpM part f = case part of
    Self -> return $ mempty
    Prior -> return $ mempty
    Proj e _ -> f e
    ProjAssign e1 _ e2 e3 -> f e1 *+* f e2 *+* f e3
    Case e brs -> foldl (*+*) (f e) $ map (\(TA.Branch _ e') -> f e') brs
    Onion e1 e2 -> f e1 *+* f e2
    Func _ e -> f e
    Appl e1 e2 -> f e1 *+* f e2
    where x *+* y = (liftM2 mappend) x y
          infixl 4 *+*

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