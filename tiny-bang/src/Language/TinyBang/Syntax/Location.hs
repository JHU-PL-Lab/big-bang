{-# LANGUAGE ViewPatterns #-}

{-|
  A module defining source locations.
-}
module Language.TinyBang.Syntax.Location
( SourceRegion(..)
, SourceLocation(..)
, SourceDocument(..)
) where

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
