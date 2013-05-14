{-|
  This module contains the Haskell data types for basic types, type variables,
  and constraints in TinyBang.  This module's constructors overlap with the
  constructors in @Language.TinyBang.Ast@ and are expected to be qualified on
  import.
-}

module Language.TinyBang.TypeSystem.Types
( Type(..)
, PatternType(..)
, InnerPatternType(..)
, CellTVar(..)
, FlowTVar(..)
, AnyTVar(..)

, Constraint(..)
, IntermediateConstraint(..)
, TypeConstraint(..)
, ApplicationConstraint(..)
, OperationConstraint(..)
, CellCreationConstraint(..)
, CellLoadingConstraint(..)
, CellSettingConstraint(..)
, FinalConstraint(..)
, ImmutableConstraint(..)
, FlowConstraint(..)
, ExceptionConstraint(..)

, ConstraintHistory(..)
, SourceElement(..)

, ConstraintDatabase(..)
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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


-- * Types

-- |Represents TinyBang types.  Parametric in the type of constraint database
--  used to store scape constraints.
data Type db
  = Primitive A.PrimitiveType
  | EmptyOnion
  | Label A.LabelName CellTVar
  | Onion FlowTVar FlowTVar
  | OnionFilter FlowTVar A.OnionOp A.Projector
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
data CellTVar = CellTVar A.CellVar PossibleContour
  deriving (Eq, Ord, Show)

-- |Represents flow type variables.
data FlowTVar = FlowTVar A.FlowVar PossibleContour
  deriving (Eq, Ord, Show)
  
-- |A wrapper for any type variable.
data AnyTVar
  = SomeCellTVar CellTVar
  | SomeFlowTVar FlowTVar
  deriving (Eq, Ord, Show)
  

-- * Constraints

-- |Represents TinyBang type constraints.  Parametric in the type of database
--  which appears in the scape types within these constraints.
data Constraint db
  = WrapIntermediateConstraint IntermediateConstraint
  | WrapTypeConstraint (TypeConstraint db)
  | WrapApplicationConstraint ApplicationConstraint
  | WrapOperationConstraint OperationConstraint
  | WrapCellCreationConstraint CellCreationConstraint
  | WrapCellLoadingConstraint CellLoadingConstraint
  | WrapCellSettingConstraint CellSettingConstraint
  | WrapFinalConstraint FinalConstraint
  | WrapImmutableConstraint ImmutableConstraint
  | WrapFlowConstraint FlowConstraint
  | WrapExceptionConstraint ExceptionConstraint
  deriving (Eq, Ord, Show)

data IntermediateConstraint = IntermediateConstraint FlowTVar FlowTVar
  deriving (Eq, Ord, Show)
  
-- See notes/TypeConstraint-Foldable.txt for a limitation
data TypeConstraint db = TypeConstraint (Type db) FlowTVar
  deriving (Eq, Ord, Show)

data ApplicationConstraint = ApplicationConstraint FlowTVar FlowTVar FlowTVar
  deriving (Eq, Ord, Show)

data OperationConstraint
  = OperationConstraint FlowTVar A.BinaryOperator FlowTVar FlowTVar
  deriving (Eq, Ord, Show)

data CellCreationConstraint = CellCreationConstraint FlowTVar CellTVar
  deriving (Eq, Ord, Show)

data CellLoadingConstraint = CellLoadingConstraint CellTVar FlowTVar
  deriving (Eq, Ord, Show)

data CellSettingConstraint = CellSettingConstraint FlowTVar CellTVar
  deriving (Eq, Ord, Show)

data FinalConstraint = FinalConstraint CellTVar
  deriving (Eq, Ord, Show)

data ImmutableConstraint = ImmutableConstraint CellTVar
  deriving (Eq, Ord, Show)

data FlowConstraint = FlowConstraint FlowTVar A.FlowKind FlowTVar
  deriving (Eq, Ord, Show)

data ExceptionConstraint = ExceptionConstraint FlowTVar FlowTVar
  deriving (Eq, Ord, Show)


-- * Constraint history

data ConstraintHistory db
  = DerivedFromSource SourceElement
  | CompatibilityWiring -- TODO: more info?
  deriving (Eq, Ord, Show)

data SourceElement
  = ClauseElement A.Clause
  | PatternElement A.Pattern
  | QualifierElement A.CellQualifier
  deriving (Eq, Ord, Show)

instance A.HasOrigin SourceElement where
  originOf x = case x of
    ClauseElement cl -> A.originOf cl
    PatternElement pat -> A.originOf pat
    QualifierElement q -> A.originOf q


-- * Constraint databases

-- |A typeclass defining the interface for constraint databases.
class (Eq db) => ConstraintDatabase db where
  -- |Obtains an empty constraint database.
  empty :: db
  -- |Adds a new constraint to a database.
  add :: Constraint db -> ConstraintHistory db -> db -> db
  -- |Unions two databases.  If there is a performance difference between the
  --  ordering of the arguments, it will generally be in favor of the first
  --  database being the larger one.
  union :: db -> db -> db
  -- |Retrieves all constraints stored in this database.
  getAllConstraints :: db -> Set (Constraint db)
  -- |Finds all lower bounds for the provided type variable.
  getLowerBounds :: FlowTVar -> db -> Set (Type db)
  -- |Finds all cell assignment and construction bounds for the provided cell
  --  variable.
  getCellOrStoreBounds :: CellTVar -> db -> Set FlowTVar
  
  -- |Performs cell substitution on a database.  The provided map is keyed by
  --  the variables to replace and valued by their replacements.
  substituteCellVariables :: Map CellTVar CellTVar -> db -> db
  
  -- |Obtains the set of contours which appear in a database.
  getAllContours :: db -> Set Contour
  -- |Instantiates contours on specific variables in a database.
  instantiateContours :: Contour -> db -> db
  -- |Performs contour replacement on the contents of a database.
  replaceContours :: Contour -> db -> db
  
  -- TODO: the rest of the interface
  
  -- ### Convenience functions
  -- |Creates a singleton constraint database.  By default, this simply adds
  --  a constraint to an empty database.
  singleton :: Constraint db -> ConstraintHistory db -> db
  singleton c h = add c h empty
  -- |Creates a constraint database from a list of constraint-history pairs.
  --  By default, this simply adds these constraints in order to an empty
  --  database.
  fromList :: [(Constraint db, ConstraintHistory db)] -> db
  fromList = foldr (uncurry add) empty

-- * Typeclass instances

instance (ConstraintDatabase db) => Display (Type db) where
  makeDoc typ = case typ of
    Primitive p -> makeDoc p
    EmptyOnion -> text "()"
    Label n b -> makeDoc n <+> makeDoc b
    Onion a1 a2 -> makeDoc a1 <+> char '&' <+> makeDoc a2
    OnionFilter a op proj -> makeDoc a <+> makeDoc op <+> makeDoc proj
    Scape tpat a cs -> makeDoc tpat <+> text "->" <+> makeDoc a <+> char '\\'
                          <+> delimFillSep lbrace rbrace comma
                                (map makeDoc $ Set.toList $
                                  getAllConstraints cs)
                          
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
  makeDoc (FlowTVar x pc) = makeDoc x <> char '^' <> makeDoc pc

instance Display CellTVar where
  makeDoc (CellTVar y pc) = makeDoc y <> char '^' <> makeDoc pc
  
subdoc :: Doc
subdoc = text "<:"
  
instance (ConstraintDatabase db) => Display (Constraint db) where
  makeDoc c = case c of
    WrapIntermediateConstraint arg -> makeDoc arg
    WrapTypeConstraint arg -> makeDoc arg
    WrapApplicationConstraint arg -> makeDoc arg
    WrapOperationConstraint arg -> makeDoc arg
    WrapCellCreationConstraint arg -> makeDoc arg
    WrapCellLoadingConstraint arg -> makeDoc arg
    WrapCellSettingConstraint arg -> makeDoc arg
    WrapFinalConstraint arg -> makeDoc arg
    WrapImmutableConstraint arg -> makeDoc arg
    WrapFlowConstraint arg -> makeDoc arg
    WrapExceptionConstraint arg -> makeDoc arg
    
instance Display IntermediateConstraint where
  makeDoc (IntermediateConstraint a a') = makeDoc a <+> subdoc <+> makeDoc a'

instance (ConstraintDatabase db) => Display (TypeConstraint db) where
  makeDoc (TypeConstraint t a) = makeDoc t <+> subdoc <+> makeDoc a
  
instance Display ApplicationConstraint where
  makeDoc (ApplicationConstraint a a' a'') =
    makeDoc a <+> makeDoc a' <+> subdoc <+> makeDoc a''

instance Display OperationConstraint where
  makeDoc (OperationConstraint a op a' a'') =
    makeDoc a <+> makeDoc op <+> makeDoc a' <+> subdoc <+> makeDoc a''

instance Display CellCreationConstraint where
  makeDoc (CellCreationConstraint a b) =
    text "cell" <+> makeDoc a <+> subdoc <+> makeDoc b

instance Display CellLoadingConstraint where
  makeDoc (CellLoadingConstraint b a) =
    text "load" <+> makeDoc b <+> subdoc <+> makeDoc a

instance Display CellSettingConstraint where
  makeDoc (CellSettingConstraint a b) =
    text "store" <+> makeDoc a <+> subdoc <+> makeDoc b

instance Display FinalConstraint where
  makeDoc (FinalConstraint b) = text "final" <+> makeDoc b

instance Display ImmutableConstraint where
  makeDoc (ImmutableConstraint b) = text "immut" <+> makeDoc b

instance Display FlowConstraint where
  makeDoc (FlowConstraint a k a') =
    makeDoc a <+> makeDoc k <> subdoc <+> makeDoc a'                                     

instance Display ExceptionConstraint where
  makeDoc (ExceptionConstraint a a') =
    text "exn" <+> makeDoc a <+> subdoc <+> makeDoc a'
