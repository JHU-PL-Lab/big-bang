{-# LANGUAGE TemplateHaskell, ViewPatterns, DataKinds, GADTs, KindSignatures, StandaloneDeriving, TupleSections #-}

{-|
  A module which defines the data structures which comprise the TinyBang ANF
  AST.
-}

module Language.TinyBang.Ast.Data
( Expr(..)
, Clause(..)
, Redex(..)
, BuiltinOp(..)
, Value(..)
, PrimitiveValue(..)
, Pattern(..)
, PatternFilterMap(..)
, Filter(..)
, PrimitiveType(..)
, LabelName(..)
, ModuleName(..)
, Var(..)
, VarName(..)

, unLabelName
, unVar

, valAsInt
, valAsChar

, mkvar
, typeOfPrimitiveValue
, exprConcat
) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
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
  = Clause Origin Var Redex
  deriving (Show)

-- |A data type representing reducible expressions.
data Redex
  = Def Origin Value
  | Copy Origin Var
  | Appl Origin Var Var
  | Builtin Origin BuiltinOp [Var]
  | Load Origin ModuleName
  deriving (Show)

-- |A data type enumerating the builtins supported by the semantics.
data BuiltinOp
  = OpIntPlus -- ^Integer addition.
  | OpIntMinus -- ^Integer subtraction.
  | OpIntMult -- ^Integer multiplication.
  | OpIntDiv -- ^Integer (rounding) division.
  | OpIntMod -- ^Integer modulus.
  | OpIntEq -- ^Integer equality.
  | OpIntLessEq -- ^Integer inequality.
  | OpIntGreaterEq -- ^Integer inequality.
  | OpSet -- ^Cell assignment.  Arguments are the cell and the value to store.
  | OpGetChar -- ^I/O character read.  No arguments.
  | OpPutChar -- ^I/O character write.  Single argument is the char to write.
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
  | VChar Origin Char
  deriving (Show)

-- |A data type describing patterns.  Patterns in this implementation are
--  represented as a variable and a mapping from variable to its corresponding
--  filters.  The origin accompanying each filter is that which identifies the
--  clause which defined it.
data Pattern
  = Pattern Origin Var PatternFilterMap
  deriving (Show)
  
newtype PatternFilterMap
  = PatternFilterMap { unPatternFilterMap :: Map Var (Origin, Filter) }
  deriving (Show)

-- |A data type describing pattern filters.
data Filter
  = FPrimitive Origin PrimitiveType
  | FEmptyOnion Origin
  | FLabel Origin LabelName Var
  | FRef Origin Var
  | FConjunction Origin Var Var
  deriving (Show)

-- |A representation of primitive types.
data PrimitiveType
  = PrimInt
  | PrimChar
  deriving (Eq, Ord, Show)

-- |A semantic wrapper for label names.
data LabelName
  = LabelName Origin String
  deriving (Show)

data ModuleName
  = ModuleName Origin [String]

instance Show ModuleName where
  show (ModuleName _ s) = concat $ intersperse "." s
  
-- |A semantic wrapper for variables.  The @Maybe Integer@ contains a freshening
--  index; it is @Nothing@ for an original (unfreshened) variable.
data Var
  = Var Origin VarName (Maybe Integer)
  deriving (Show)
  
-- |A description of variable names.  This includes variables from syntax and
--  globally unique variables, such as those used by builtin operations.
data VarName
  = IdentifierVar String
      -- ^The type for normal variables like the programmer would use.
  | BuiltinVar BuiltinOp
      -- ^The type for variables used as the output for built-in operations.
  | PrimitiveMatchPatternVar PrimitiveType
      -- ^The type for variables used in fabricated patterns for the purpose of
      --  extracting a specific primitive type.
  | RefMatchPatternVar Int
      -- ^The type for variables used in fabricated patterns for the purpose of
      --  extracting the contents of a ref.
  deriving (Eq, Ord, Show)

-- * Destructors

unLabelName :: LabelName -> String
unLabelName (LabelName _ s) = s

unVar :: Var -> VarName
unVar (Var _ n _) = n
  
valAsInt :: Value -> Maybe Integer
valAsInt v = case v of
  VPrimitive _ (VInt _ n) -> Just n
  _ -> Nothing

valAsChar :: Value -> Maybe Char
valAsChar v = case v of
  VPrimitive _ (VChar _ n) -> Just n
  _ -> Nothing

-- * Generally related routines

mkvar :: Origin -> String -> Var
mkvar o s = Var o (IdentifierVar s) Nothing

typeOfPrimitiveValue :: PrimitiveValue -> PrimitiveType
typeOfPrimitiveValue v = case v of
  VInt _ _ -> PrimInt
  VChar _ _ -> PrimChar

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
    Clause _ x r -> makeDoc x <+> text "=" <+> makeDoc r

instance Display Redex where
  makeDoc r = case r of
    Def _ v -> makeDoc v
    Copy _ x -> makeDoc x
    Appl _ x x' -> makeDoc x <+> makeDoc x'
    Builtin _ bop xs ->
      makeDoc bop <+> sepDoc (char ' ') (map makeDoc xs)
    Load _ mn -> text "load" <+> makeDoc mn

instance Display BuiltinOp where
  makeDoc o = case o of
    OpIntPlus -> char '+'
    OpIntMinus -> char '-'
    OpIntMult -> char '*'
    OpIntDiv -> char '/'
    OpIntMod -> char '%'
    OpIntEq -> text "=="
    OpIntLessEq -> text "<="
    OpIntGreaterEq -> text ">="
    OpSet -> text "<-"
    OpGetChar -> text "getChar"
    OpPutChar -> text "putChar"
    
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
    VChar _ n -> text $ show n

instance Display Pattern where
  makeDoc pat = case pat of
    Pattern _ x pfm ->
      makeDoc x <+> char '\\' <+> makeDoc pfm 

instance Display PatternFilterMap where
  makeDoc (PatternFilterMap pfm) = 
    char '{' <+>
      sepDoc (text ";") (map mappingDoc $ Map.toList pfm) <+> char '}'
    where
      mappingDoc (x,(_,pf)) = makeDoc x <+> char '=' <+> makeDoc pf

instance Display Filter where
  makeDoc pv = case pv of
    FPrimitive _ n -> makeDoc n
    FEmptyOnion _ -> text "()"
    FLabel _ n x -> makeDoc n <+> makeDoc x
    FRef _ x -> text "ref" <+> makeDoc x
    FConjunction _ x x' -> makeDoc x <+> text "*" <+> makeDoc x'

instance Display PrimitiveType where
  makeDoc p = case p of
    PrimInt -> text "int"
    PrimChar -> text "char"

instance Display LabelName where
  makeDoc n = text "`" <> text (unLabelName n)

instance Display ModuleName where
  makeDoc n = case n of
    ModuleName _ i -> foldl1 (<>) (map (\x -> text "." <> text x) i)
  
instance Display Var where
  makeDoc (Var _ n mf) =
    makeDoc n <> maybe empty (\i -> char '#' <> text (show i)) mf

instance Display VarName where
  makeDoc n = case n of
    IdentifierVar s -> text s
    BuiltinVar op -> parens (makeDoc op <+> text "var")
    PrimitiveMatchPatternVar tprim -> parens (makeDoc tprim <+> text "match")
    RefMatchPatternVar i -> parens (text "ref match" <+> makeDoc i)

-- * Appropriate @Eq@ and @Ord@ instances for these data types

$(concat <$> sequence
  [ f name
  | f <- [deriveEqSkipFirst, deriveOrdSkipFirst]
  , name <-
      [ ''Expr
      , ''Clause
      , ''Redex
      , ''Value
      , ''PrimitiveValue
      , ''Pattern
      , ''Filter
      , ''LabelName
      , ''ModuleName
      , ''Var
      ]
  ])

deriving instance Eq PatternFilterMap  
deriving instance Ord PatternFilterMap

-- * @HasOrigin@ declarations for the AST types.
-- TODO: metaprogram these

instance HasOrigin Expr where
  originOf x = case x of
    Expr orig _ -> orig

instance HasOrigin Clause where
  originOf x = case x of
    Clause orig _ _ -> orig

instance HasOrigin Redex where
  originOf x = case x of
    Def orig _ -> orig
    Copy orig _ -> orig
    Appl orig _ _ -> orig
    Builtin orig _ _ -> orig
    Load orig _ -> orig

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
    VChar orig _ -> orig

instance HasOrigin Pattern where
  originOf x = case x of
    Pattern orig _ _ -> orig

instance HasOrigin LabelName where
  originOf x = case x of
    LabelName orig _ -> orig

instance HasOrigin ModuleName where
  originOf x = case x of
    ModuleName orig _ -> orig

instance HasOrigin Var where
  originOf x = case x of
    Var orig _ _ -> orig
