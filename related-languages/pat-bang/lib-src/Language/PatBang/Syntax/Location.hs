{-|
  A module defining source locations.
-}
module Language.PatBang.Syntax.Location
( SourceRegion(..)
, SourceLocation(..)
, SourceDocument(..)
) where

import Language.PatBang.Display hiding (line)

-- |Defines a region of source code.
data SourceRegion
  = SourceRegion -- ^ Signifies a source region over known locations
      SourceLocation -- ^ The start position
      SourceLocation -- ^ The end position
  deriving (Eq, Ord, Show)

-- |Defines a data type for text positions.  The @lineNo@ is one-based; the
--  @colNo@ is zero-based.
data SourceLocation
  = TextSource
      { textSourceDocument :: SourceDocument
      , textSourceLineNo :: Int
      , textSourceColNo :: Int }
  | Unknown
  deriving (Eq, Ord, Show)

-- |Defines a data type describing source code documents.  A source document is
--  some resource which contains the source code in textual form.
data SourceDocument
  = UnknownDocument
  deriving (Eq, Ord, Show)


instance Display SourceRegion where
  makeDoc (SourceRegion start end) =
    case (start, end) of
      (TextSource doc line col, TextSource doc' line' col') | doc == doc' ->
        if line == line' then
          if col == col' then
            makeDoc start
          else
            makeDoc doc <> char '@' <> makeDoc line <> makeDoc ':'
              <> makeDoc col <> char '-' <> makeDoc col'
        else
          makeDoc doc <> char '@' <> makeDoc line <> char ':' <> makeDoc col
            <> char '-' <> makeDoc line' <> char ':' <> makeDoc col'
      _ -> makeDoc start <> char '-' <> makeDoc end

instance Display SourceLocation where
  makeDoc sl = case sl of
    TextSource doc line col ->
      makeDoc doc <> char '@' <> makeDoc line <> char '-' <> makeDoc col
    Unknown ->
      text "(unknown)"

instance Display SourceDocument where
  makeDoc sd = case sd of
    UnknownDocument -> text "(unknown)"
