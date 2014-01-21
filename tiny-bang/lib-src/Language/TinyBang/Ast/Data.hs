{-# LANGUAGE TemplateHaskell, ViewPatterns, DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}

{-|
  A module which defines the data structures which comprise the TinyBang ANF
  AST.
-}

module Language.TinyBang.Ast.Data
( Expr(..)
, Clause(..)
, EvaluatedClause(..)
, Redex(..)
, BuiltinOp(..)
, Value(..)
, PrimitiveValue(..)
, Pattern(..)
, PatternClause(..)
, PatternValue(..)
, PrimitiveType(..)
, LabelName(..)
, Var(..)

, unLabelName
, unVar

, valAsInt

, typeOfPrimitiveValue
, exprConcat
) where

import Control.Applicative ((<$>))
import Text.PrettyPrint.Leijen hiding ((<$>),list)

import Language.TinyBang.Ast.Origin
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.TemplateHaskell.Deriving

-- * Basic data structures

-- |A data type representing expressions.
data Expr
  = Expr Origin [Clause]
  deriving (Show)

-- |A data type representing general clauses.
data Clause
  = Evaluated EvaluatedClause
  | RedexDef Origin Var Redex
  deriving (Show)

-- |A data type representing evaluated clauses.
data EvaluatedClause
  = ValueDef Origin Var Value
  deriving (Show)

-- |A data type representing reducible expressions.
data Redex
  = Define Origin Var
  | Appl Origin Var Var
  | Builtin Origin BuiltinOp [Var]
  deriving (Show)

-- |A data type enumerating the builtins supported by the semantics.
data BuiltinOp
  = OpIntPlus
  | OpIntMinus
  | OpIntEq
  | OpIntLessEq
  | OpIntGreaterEq
  | OpSet
  deriving (Eq, Ord, Show, Enum, Bounded) 

-- |A data type representing value forms.
data Value
  = VPrimitive Origin PrimitiveValue
  | VEmptyOnion Origin
  | VLabel Origin LabelName Var
  | VRef Origin Var
  | VOnion Origin Var Var
  | VScape Origin Pattern Expr
  deriving (Show)

-- |A data type representing primitive values.
data PrimitiveValue
  = VInt Origin Integer
  deriving (Show)

-- |A data type describing patterns.
data Pattern
  = Pattern Origin [PatternClause]
  deriving (Show)

-- |A data type describing pattern clauses.
data PatternClause
  = PatternClause Origin Var PatternValue
  deriving (Show)

-- |A data type describing pattern values.
data PatternValue
  = PPrimitive Origin PrimitiveType
  | PEmptyOnion Origin
  | PLabel Origin LabelName Var
  | PRef Origin Var
  | PConjunction Origin Var Var
  deriving (Show)

-- |A representation of primitive types.
data PrimitiveType
  = PrimInt
  deriving (Eq, Ord, Show)

-- |A semantic wrapper for label names.
data LabelName
  = LabelName Origin String
  deriving (Show)

-- |A semantic wrapper for variables.
data Var
  = Var Origin String
  | GenVar Origin String Integer
      -- ^Generated variables.  Arguments are the name of the original variable
      --  and the generation index.
  deriving (Show)

-- * Destructors

unLabelName :: LabelName -> String
unLabelName (LabelName _ s) = s

unVar :: Var -> String
unVar x = case x of
  Var _ s -> s
  GenVar _ s n -> s ++ "__" ++ show n
  
valAsInt :: Value -> Maybe Integer
valAsInt v = case v of
  VPrimitive _ (VInt _ n) -> Just n
  _ -> Nothing

-- * Generally related routines

typeOfPrimitiveValue :: PrimitiveValue -> PrimitiveType
typeOfPrimitiveValue v = case v of
  VInt _ _ -> PrimInt

exprConcat :: Expr -> Expr -> Expr
-- TODO: better origin!
exprConcat (Expr _ cls1) (Expr _ cls2) = Expr generated $ cls1 ++ cls2 

-- * Display instances for the above types
-- TODO: include positional information somehow (via annotations?)
-- TODO: separate pretty printer?

instance Display Expr where
  makeDoc (Expr _ cls) = sepDoc (text "; ") $ map makeDoc cls

instance Display Clause where
  makeDoc cl = case cl of
    RedexDef _ x r -> makeDoc x <+> text "=" <+> makeDoc r
    Evaluated ecl -> makeDoc ecl
    
instance Display EvaluatedClause where
  makeDoc ecl = case ecl of
    ValueDef _ x v -> makeDoc x <+> text "=" <+> makeDoc v

instance Display Redex where
  makeDoc r = case r of
    Define _ x -> makeDoc x
    Appl _ x x' -> makeDoc x <+> makeDoc x'
    Builtin _ bop xs ->
      makeDoc bop <+> sepDoc (char ' ') (map makeDoc xs)

instance Display BuiltinOp where
  makeDoc o = case o of
    OpIntPlus -> char '+'
    OpIntMinus -> char '-'
    OpIntEq -> text "=="
    OpIntLessEq -> text "<="
    OpIntGreaterEq -> text ">="
    OpSet -> text "<-"
    
instance Display Value where
  makeDoc v = case v of
    VPrimitive _ n -> makeDoc n
    VEmptyOnion _ -> text "()"
    VLabel _ n x -> makeDoc n <+> makeDoc x
    VRef _ x -> text "ref" <+> makeDoc x
    VOnion _ x x' -> makeDoc x <+> text "&" <+> makeDoc x'
    VScape _ pat e -> makeDoc pat <+> text "->" <+> text "{" <+> makeDoc e
                          <+> text "}"

instance Display PrimitiveValue where
  makeDoc v = case v of
    VInt _ n -> text $ show n

instance Display Pattern where
  makeDoc pat = case pat of
    Pattern _ pcls ->
      text "{" <+> sepDoc (text ";") (map makeDoc pcls) <+> text "}"

instance Display PatternClause where
  makeDoc pcl = case pcl of
    PatternClause _ x pv ->
      makeDoc x <+> text "=" <+> makeDoc pv
      
instance Display PatternValue where
  makeDoc pv = case pv of
    PPrimitive _ n -> makeDoc n
    PEmptyOnion _ -> text "()"
    PLabel _ n x -> makeDoc n <+> makeDoc x
    PRef _ x -> text "ref" <+> makeDoc x
    PConjunction _ x x' -> makeDoc x <+> text "&" <+> makeDoc x'

instance Display PrimitiveType where
  makeDoc p = case p of
    PrimInt -> text "int"

instance Display LabelName where
  makeDoc n = text "`" <> text (unLabelName n)
  
instance Display Var where
  makeDoc x = text $ unVar x

-- * Appropriate @Eq@ and @Ord@ instances for these data types

$(concat <$> sequence
  [ f name
  | f <- [deriveEqSkipFirst, deriveOrdSkipFirst]
  , name <-
      [ ''Expr
      , ''Clause
      , ''EvaluatedClause
      , ''Redex
      , ''Value
      , ''PrimitiveValue
      , ''Pattern
      , ''PatternClause
      , ''PatternValue
      , ''LabelName
      , ''Var
      ]
  ])

-- * @HasOrigin@ declarations for the AST types.
-- TODO: metaprogram these

instance HasOrigin Expr where
  originOf x = case x of
    Expr orig _ -> orig

instance HasOrigin Clause where
  originOf x = case x of
    RedexDef orig _ _ -> orig
    Evaluated c -> originOf c

instance HasOrigin EvaluatedClause where
  originOf x = case x of
    ValueDef orig _ _ -> orig

instance HasOrigin Redex where
  originOf x = case x of
    Define orig _ -> orig
    Appl orig _ _ -> orig
    Builtin orig _ _ -> orig

instance HasOrigin Value where
  originOf x = case x of
    VPrimitive orig _ -> orig
    VEmptyOnion orig -> orig
    VLabel orig _ _ -> orig
    VRef orig _ -> orig
    VOnion orig _ _ -> orig
    VScape orig _ _ -> orig

instance HasOrigin PrimitiveValue where
  originOf x = case x of
    VInt orig _ -> orig

instance HasOrigin Pattern where
  originOf x = case x of
    Pattern orig _ -> orig

instance HasOrigin PatternClause where
  originOf x = case x of
    PatternClause orig _ _ -> orig

instance HasOrigin LabelName where
  originOf x = case x of
    LabelName orig _ -> orig

instance HasOrigin Var where
  originOf x = case x of
    Var orig _ -> orig
    GenVar orig _ _ -> orig
