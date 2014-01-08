{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving #-} 

{-|
  This module contains the Haskell data types for basic types and type variables
  in TinyBang.
-}
module Language.TinyBang.TypeSystem.Types
( TypeOrVar(..)
, unTypeOrVar
, mktov
, Type(..)
, TVar(..)
, contourOfVar
) where

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.Utils.Display

newtype TypeOrVar db = TypeOrVar (Either (Type db) TVar)
  deriving (Show)
  
unTypeOrVar :: TypeOrVar db -> Either (Type db) TVar
unTypeOrVar (TypeOrVar x) = x

class TypeOrVarContents a db where
  mktov :: a -> TypeOrVar db
instance TypeOrVarContents (Type db) db where
  mktov = TypeOrVar . Left
instance TypeOrVarContents TVar db where
  mktov = TypeOrVar . Right

-- |The Haskell data type representing TinyBang types.  This type is
--  parameterized in the type of constraint database which appears in scapes.
data Type db
  = TPrimitive PrimitiveType
  | TEmptyOnion
  | TLabel LabelName (TypeOrVar db)
  | TOnion (TypeOrVar db) (TypeOrVar db)
  | TScape TVar db TVar db
  deriving (Show)
  
-- |The Haskell data type representing TinyBang type variables.
data TVar
  = TVar Var PossibleContour
  deriving (Eq, Ord, Show)

-- |Retrieves the contour of a variable (if it has one).
contourOfVar :: TVar -> Maybe Contour
contourOfVar (TVar _ pcntr) = unPossibleContour pcntr

-- * Display instances

instance (Display db) => Display (TypeOrVar db) where
  makeDoc = either makeDoc makeDoc . unTypeOrVar

instance (Display db) => Display (Type db) where
  makeDoc t =
    case t of
      TPrimitive pt -> makeDoc pt
      TEmptyOnion -> text "()"
      TLabel n tov -> makeDoc n <+> recurse tov
      TOnion tov1 tov2 -> recurse tov1 <+> char '&' <+> recurse tov2
      TScape a' cs' a cs ->
        makeDoc a' <> char '\\' <> makeDoc cs' <+> text "->" <+>
          makeDoc a <> char '\\' <> makeDoc cs
    where
      recurse tov =
        case unTypeOrVar tov of
          Left t' ->
            (if prio t > prio t' then parens else id) $ makeDoc t'
          Right a -> makeDoc a
      prio :: Type db -> Int
      prio t' = case t' of
        TPrimitive _ -> atom
        TEmptyOnion -> atom
        TLabel{} -> 8
        TOnion{} -> 4
        TScape{} -> 0
        where atom = 9999

instance Display TVar where
  makeDoc (TVar x pc) =
    char 'α' <> makeDoc x <> char '^' <>
      maybe (char '*') makeDoc (unPossibleContour pc)

deriving instance (Eq db) => Eq (Type db)
deriving instance (Ord db) => Ord (Type db)
deriving instance (Eq db) => Eq (TypeOrVar db)
deriving instance (Ord db) => Ord (TypeOrVar db)
