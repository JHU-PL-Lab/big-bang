{-|
  A module defining the @Origin@ data type.  This type tracks the circumstances
  under which AST components were constructed.
-}
module Language.TinyBang.Ast.Origin
( Origin(..)
, HasOrigin(..)
, generated
) where

import Language.TinyBang.Utils.Display
import Language.TinyBang.Syntax.Location

-- |The data structure describing AST component origins.
data Origin
  = SourceOrigin SourceSpan
    -- ^ Signifies an AST node which originates from source code.
  | ComputedOrigin [Origin]
    -- ^ Signifies an AST node which originates from the computation of other
    --   nodes.  The list contains the origins of the nodes involved in the
    --   computation.
  deriving (Eq, Ord, Show)

instance Display Origin where
  makeDoc orig = case orig of
    SourceOrigin sr -> makeDoc sr
    ComputedOrigin origs -> text "(" <> text "computed from" <+>
      makeDoc (concatMap flattenOrigins origs) <> text ")"
    where
      flattenOrigins :: Origin -> [SourceSpan]
      flattenOrigins orig' = case orig' of
        SourceOrigin sr -> [sr]
        ComputedOrigin origs -> concatMap flattenOrigins origs

-- |A typeclass for constructs containing a definitive source region.
class HasOrigin a where
  originOf :: a -> Origin

instance HasOrigin Origin where
  originOf = id
  
-- * Smart constructors

generated :: Origin
generated = ComputedOrigin []

