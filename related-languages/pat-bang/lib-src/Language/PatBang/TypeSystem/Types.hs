{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-} 

{-|
  This module contains the Haskell data types for basic types, type variables,
  and constraints in PatBang.  This module's constructors overlap with the
  constructors in @Language.PatBang.Ast@ and are expected to be qualified on
  import.
-}

module Language.PatBang.TypeSystem.Types
( Type(..)
, PatternBody(..)
, FlowTVar(..)
, PatTVar(..)
) where

import qualified Language.PatBang.Ast as A
import Language.PatBang.Display
  ( Display(..)
  , (<>)
  , (<+>)
  , text
  , char
  , parens
  , lbrace
  , rbrace
  , comma
  , delimFillSep
  , delimSepDoc
  , sepDoc
  , Doc)
import Language.PatBang.TypeSystem.Contours
import Language.PatBang.TypeSystem.Utils.DocumentContainer

-- |Represents PatBang types.  Parametric in the type of constraint database
--  used to store scape constraints.
data Type db
  = Primitive A.PrimitiveType
  | EmptyOnion
  | Label A.LabelName FlowTVar
  | Onion FlowTVar FlowTVar
  | Function [FlowTVar] FlowTVar db
  | Pattern [PatTVar] PatternBody
  | Scape FlowTVar FlowTVar 
  deriving (Eq, Ord, Show)
  
data PatternBody
  = PPrim A.PrimitiveType
  | PLabel A.LabelName PatternBody
  | PFun
  | PPat
  | PScape
  | PConj PatternBody PatternBody
  | PSubst FlowTVar [PatternBody]
  | PRec PatTVar PatternBody
  | PVar PatTVar
  deriving (Eq, Ord, Show)

-- |Represents flow type variables.
data FlowTVar
  = FlowTVar A.FlowVar PossibleContour
      -- ^ Standard flow type variables
  | GenFlowTVar A.FlowVar PossibleContour
      -- ^ Flow type variables modeling the @FVar@ function in the PatBang spec
  deriving (Eq, Ord, Show)
  
-- |Represents pattern type variables.
data PatTVar = PatTVar A.PatVar
  deriving (Eq, Ord, Show)

instance Display PatternBody where
  makeDoc tpat = case tpat of
    PPrim t -> makeDoc t
    PLabel n tpat' -> makeDoc n <+> recurse tpat'
    PFun -> makeDoc "fun"
    PPat -> makeDoc "pat"
    PScape -> makeDoc "scape"
    PConj tpat' tpat'' -> recurse tpat' <+> char '&' <+> recurse tpat''
    PSubst a tpats -> makeDoc a <+> char '('
                        <> sepDoc (text ", ") (map makeDoc tpats) <> char ')'
    PRec b tpat' -> text "rec" <+> makeDoc b <> char ':' <+> recurse tpat'
    PVar b -> makeDoc b
    where
      precedence :: PatternBody -> Int
      precedence tpat' = case tpat' of
        PPrim _ -> atom
        PLabel _ _ -> 9
        PFun -> atom
        PPat -> atom
        PScape -> atom
        PConj _ _ -> 4
        PSubst _ _ -> 15
        PRec _ _ -> 0
        PVar _ -> atom
        where
          atom = -9999
      recurse :: PatternBody -> Doc
      recurse tpat' =
        (if precedence tpat > precedence tpat'
          then parens
          else id)
            $ makeDoc tpat'

instance Display FlowTVar where
  makeDoc a = case a of
    FlowTVar x pc -> makeDoc x <> char '^' <> makeDoc pc
    GenFlowTVar x pc -> makeDoc x <> text "#F^" <> makeDoc pc

instance Display PatTVar where
  makeDoc b = case b of
    PatTVar y -> makeDoc y

instance (DocumentContainer db) => Display (Type db) where
  makeDoc typ = case typ of
    Primitive p -> makeDoc p
    EmptyOnion -> text "()"
    Label n b -> makeDoc n <+> makeDoc b
    Onion a1 a2 -> makeDoc a1 <+> char '&' <+> makeDoc a2
    Function aa a cs ->
      delimSepDoc (char '(') (char ')') (text ", ") (map makeDoc aa)
        <+> text "->" <+> makeDoc a <+> char '\\'
        <+> delimFillSep lbrace rbrace comma (getContainedDocuments cs)
    Pattern bs tpat ->
      delimSepDoc (char '(') (char ')') (text ", ") (map makeDoc bs)
        <+> text "<-" <+> makeDoc tpat
    Scape a1 a2 -> makeDoc a1 <+> text "><" <+> makeDoc a2
