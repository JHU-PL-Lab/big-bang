{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds, FlexibleInstances #-}

{-|
  This module contains general utilities for building a Parsec parser for
  data types which retain @DocumentPosition@ information.  The expected usage
  pattern is to
    * Define a parser monad to use with @ParsecT@ which implements
      @PositionalParserMonad@.
    * Define a state type to use with @ParsecT@ which implements
      @HasPositionalState@.  (Parsers which do not need a parser state may use
      the @SimplePositionalState@ type.)
    * Ensure that all raw token consumption goes through @require@ (since
      @require@ updates the parser state to record the most recently-consumed
      token).
  Doing so allows routines such as @positionParser@ to add accurate position
  information to any parser conforming to the above constraints.
-}
module Language.TinyBang.Utils.Parser.PositionalParser
( HasPositionalState(..)
, SimplePositionalState(..)
, PositionalParserConstraints
, PositionalParserMonad(..)
, positionParser
, peek
, nextPosition
, prevPosition
, require
) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Control.Monad.Trans.Class
import Data.Monoid
import Text.Parsec
import Text.Parsec.Pos

import Language.TinyBang.Syntax.Location
import Language.TinyBang.Utils.Display

-- |A typeclass which must be implemented by the state mechanism in a positional
--  parser.
class HasPositionalState a t | a -> t where
  -- |Retrieves the last token used by the parser.  Returns @Nothing@ if no
  --  token has been consumed.
  getLastToken :: a -> Maybe t
  -- |Sets the last token to be used by the parser.
  setLastToken :: t -> a -> a
  
newtype SimplePositionalState t = SimplePositionalState (Maybe t)

instance HasPositionalState (SimplePositionalState t) t where
  getLastToken (SimplePositionalState t) = t
  setLastToken t _ = SimplePositionalState $ Just t

instance Monoid (SimplePositionalState t) where
  mempty = SimplePositionalState Nothing
  mappend = flip const

type PositionalParserConstraints s u m c t =
  ( PositionalParserMonad m c
  , Stream s m t
  , HasDocumentStartStopPositions t
  , Display t
  , HasPositionalState u t)

class PositionalParserMonad m c | m -> c where
  parserSourceDocument :: m SourceDocument
  parserContext :: m c
  runPositionalParser :: m a -> c -> a

-- |Adds start and stop position data to a parser.  If the parser consumes no
--  tokens, the positional information is @Nothing@.
positionParser :: (PositionalParserConstraints s u m c t)
               => ParsecT s u m a
               -> ParsecT s u m (a, Maybe (DocumentPosition, DocumentPosition))
positionParser p = do
  mstart <- nextPosition
  x <- p
  mstop <- prevPosition
  return (x, (,) <$> mstart <*> mstop)

-- |Retrieves the next token without consuming it.  If no tokens have been
--  consumed, returns @Nothing@.
peek :: (PositionalParserConstraints s u m c t) => ParsecT s u m (Maybe t)
peek = lookAhead (Just <$> require Just) <|> return Nothing

-- |Obtains the start location of the next token.  If no such token exists,
--  returns @Nothing@.
nextPosition :: (PositionalParserConstraints s u m c t)
             => ParsecT s u m (Maybe DocumentPosition)
nextPosition = liftM documentStartPositionOf <$> peek

-- |Obtains the stop location of the most recently consumed token.  If no tokens
--  have been consumed, returns @Nothing@.
prevPosition :: (PositionalParserConstraints s u m c t)
             => ParsecT s u m (Maybe DocumentPosition)
prevPosition = liftM documentStopPositionOf <$> getLastToken <$> getState

-- |Requires that the provided token meet a given predicate function.  Showing
--  the token is accomplished with @display@.  Because the token has position,
--  this information is used to update the Parsec positional state to ensure
--  that error messages are correct.
require :: forall s u m a c t.
           (PositionalParserConstraints s u m c t)
        => (t -> Maybe a)
        -> ParsecT s u m a
require f = do
  context <- lift parserContext
  (a, t) <- tokenPrim display (nextPos context) matchToken
  modifyState $ setLastToken t
  return a
  where
    nextPos :: c
            -> SourcePos
            -> t
            -> s
            -> SourcePos
    nextPos context curPos tok toks =
      let docPos = maybe
                      (documentStopPositionOf tok)
                      (documentStartPositionOf . fst) $
                        runPositionalParser (uncons toks :: m (Maybe (t, s)))
                          context
      in
      let docName = sourceName curPos in
      case docPos of
        DocumentPosition lineNum colNum -> newPos docName lineNum colNum
    matchToken :: t -> Maybe (a, t)
    matchToken ptok = do -- Maybe
     result <- f ptok
     return (result, ptok)
