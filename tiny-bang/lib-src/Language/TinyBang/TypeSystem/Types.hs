{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-} 

{-|
  This module contains the Haskell data types for basic types, type variables,
  and constraints in TinyBang.  This module's constructors overlap with the
  constructors in @Language.TinyBang.Ast@ and are expected to be qualified on
  import.
-}

module Language.TinyBang.TypeSystem.Types
where-- TODO: rewrite
{-
( Type(..)
, PatternType(..)
, InnerPatternType(..)
, CellTVar(..)
, FlowTVar(..)
, AnyTVar(..)
) where

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.Display
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
  , Doc)
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Utils.DocumentContainer

-- |Represents TinyBang types.  Parametric in the type of constraint database
--  used to store scape constraints.
data Type db
  = Primitive A.PrimitiveType
  | EmptyOnion
  | Label A.LabelName CellTVar
  | Onion FlowTVar FlowTVar
  | OnionFilter FlowTVar A.OnionOp A.AnyProjector
  | Scape PatternType FlowTVar db 
  deriving (Eq, Ord, Show)

-- |Represents TinyBang pattern types.
data PatternType
  = ValuePattern CellTVar InnerPatternType
  | ExnPattern CellTVar InnerPatternType
  deriving (Eq, Ord, Show)

-- |Represents TinyBang inner pattern types.
data InnerPatternType
  = PrimitivePattern A.PrimitiveType
  | LabelPattern A.LabelName CellTVar InnerPatternType
  | ConjunctivePattern InnerPatternType InnerPatternType
  | ScapePattern
  | EmptyOnionPattern
  deriving (Eq, Ord, Show)

-- |Represents cell type variables.
data CellTVar
  = CellTVar A.CellVar PossibleContour
      -- ^ Standard cell type variables
  | GenCellTVar A.FlowVar PossibleContour
      -- ^ Cell type variables modeling the @CVar@ function in the TinyBang spec
  deriving (Eq, Ord, Show)
  
-- |Represents flow type variables.
data FlowTVar
  = FlowTVar A.FlowVar PossibleContour
      -- ^ Standard flow type variables
  | GenFlowTVar A.FlowVar PossibleContour
      -- ^ Flow type variables modeling the @FVar@ function in the TinyBang spec
  deriving (Eq, Ord, Show)
  
-- |A wrapper for any type variable.
data AnyTVar
  = SomeCellTVar CellTVar
  | SomeFlowTVar FlowTVar
  deriving (Eq, Ord, Show)
  
instance Display PatternType where
  makeDoc tpat = case tpat of
    ValuePattern a tipat -> makeDoc a <> char '~' <> makeDoc tipat
    ExnPattern a tipat -> text "exn" <+> makeDoc a <> char '~' <> makeDoc tipat
    
instance Display InnerPatternType where
  makeDoc = makeDoc'
    where
      makeDoc' tipat = case tipat of
        PrimitivePattern p -> makeDoc p
        LabelPattern n b tipat' -> makeDoc n <+> makeDoc b <> char '~'
                                      <> recurse tipat'
        ConjunctivePattern tipat1 tipat2 -> recurse tipat1 <+> char '&'
                                              <+> recurse tipat2
        ScapePattern -> text "fun"
        EmptyOnionPattern -> text "()"
        where
          precedence :: InnerPatternType -> Int
          precedence tipat' = case tipat' of
            PrimitivePattern _ -> atom
            LabelPattern _ _ _ -> 9
            ConjunctivePattern _ _ -> 4
            ScapePattern -> atom
            EmptyOnionPattern ->  atom
            where
              atom = -9999
          recurse :: InnerPatternType -> Doc
          recurse tipat' =
            (if precedence tipat > precedence tipat'
              then parens
              else id)
                $ makeDoc tipat'

instance Display FlowTVar where
  makeDoc a = case a of
    FlowTVar x pc -> makeDoc x <> char '^' <> makeDoc pc
    GenFlowTVar x pc -> makeDoc x <> text "#F^" <> makeDoc pc

instance Display CellTVar where
  makeDoc b = case b of
    CellTVar y pc -> makeDoc y <> char '^' <> makeDoc pc
    GenCellTVar x pc -> makeDoc x <> text "#C^" <> makeDoc pc

instance Display AnyTVar where
  makeDoc v = case v of
    SomeFlowTVar a -> makeDoc a
    SomeCellTVar b -> makeDoc b
  
instance (DocumentContainer db) => Display (Type db) where
  makeDoc typ = case typ of
    Primitive p -> makeDoc p
    EmptyOnion -> text "()"
    Label n b -> makeDoc n <+> makeDoc b
    Onion a1 a2 -> makeDoc a1 <+> char '&' <+> makeDoc a2
    OnionFilter a op proj -> makeDoc a <+> makeDoc op <+> makeDoc proj
    Scape tpat a cs -> makeDoc tpat <+> text "->" <+> makeDoc a <+> char '\\'
                          <+> delimFillSep lbrace rbrace comma
                                (getContainedDocuments cs)
-}