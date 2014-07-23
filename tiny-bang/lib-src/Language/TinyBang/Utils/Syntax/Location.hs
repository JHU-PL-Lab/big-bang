{-|
  A module defining source locations.
-}
module Language.TinyBang.Utils.Syntax.Location
( SourceSpan(..)
, HasSourceSpan(..)
, DocumentPosition(..)
, SourceDocument(..)
, nameOfDocument
, (<-->)
) where

import Language.TinyBang.Utils.Display hiding (line)

-- |Defines positions within textual documents.
data DocumentPosition
  = DocumentPosition
      { lineNo :: Int
      , colNo :: Int
      }
  deriving (Eq, Ord, Show)
  
-- |Defines a region of source.
data SourceSpan
  = DocumentSpan SourceDocument DocumentPosition DocumentPosition
      -- ^Defines a span over a given document from beginning to end
      --  (inclusive).
  | UnknownSpan
  deriving (Eq, Ord, Show)
  
-- |Defines a data type describing source code documents.  A source document is
--  some resource which contains the source code in textual form.
data SourceDocument
  = NamedDocument String
      -- ^A named document, such as the path name of a source file.
  | UnknownDocument
  deriving (Eq, Ord, Show)

class HasSourceSpan a where
  spanOf :: a -> SourceSpan

instance HasSourceSpan SourceSpan where
  spanOf = id
  
-- |Defines an operator which extracts and joins source spans.
(<-->) :: (HasSourceSpan a, HasSourceSpan b) => a -> b -> SourceSpan
(<-->) a b =
  case (spanOf a, spanOf b) of
    (DocumentSpan doc1 p1l p1r, DocumentSpan doc2 p2l p2r) | doc1 == doc2 ->
      -- Assuming that p1l <= p1r and that p2l <= p2r
      DocumentSpan doc1 (p1l `min` p2l) (p1r `max` p2r)
    _ -> UnknownSpan
infixl 7 <-->
    
-- |Obtains a suitable (but not necessarily unique) way of naming a document.
nameOfDocument :: SourceDocument -> String
nameOfDocument doc = case doc of
    NamedDocument s -> s
    UnknownDocument -> "(unknown)"

instance Display DocumentPosition where
  makeDoc (DocumentPosition line col) = makeDoc line <> char ':' <> makeDoc col

instance Display SourceSpan where
  makeDoc s = case s of
    DocumentSpan doc start stop ->
      makeDoc doc <> char '@' <> makeDoc start <> char '-' <> makeDoc stop
    UnknownSpan -> text "(unknown)"

instance Display SourceDocument where
  makeDoc doc = text $ nameOfDocument doc
