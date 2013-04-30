{-|
  This module contains an assortment of Parsec-related utilities.
-}
module Language.TinyBang.Utils.Parsec
( (</>)
, packrat
, (<@>)
) where

import Debug.Trace
import Text.Parsec.Prim

import Language.TinyBang.Display hiding ((</>))

-- |A binary packrat parser operation.  The first parser is attempted; if it
--  fails, it consumes no input and the second parser is used instead.
packrat :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
packrat a b = try a <|> b

-- |An infix alias for @packrat@.
(</>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
(</>) = packrat
infixl 1 </>

-- |Defines a parsing combinator which wraps the parser behavior in a debug
--  message routine.
(<@>) :: (Monad m, Display a) => String -> ParsecT s u m a -> ParsecT s u m a
-- TODO: fix debugging
-- The following is a crude parser-annotating operation for debugging purposes.
-- It should not use Debug.Trace; we should instead be using HLog or something.
-- TODO: assign <@> as an alias for some named function
(<@>) s p = do
  st <- getParserState
  let str = "Trying " ++ s ++ " at " ++ show (statePos st)
  x <- trace str p
  trace ("Found " ++ s ++ ": " ++ display x) $ return x

--(<@>) _ p = p
infixl 0 <@>
