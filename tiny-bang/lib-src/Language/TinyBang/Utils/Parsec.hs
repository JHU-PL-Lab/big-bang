{-# LANGUAGE TemplateHaskell #-}

{-|
  This module contains an assortment of Parsec-related utilities.
-}
module Language.TinyBang.Utils.Parsec
( (</>)
, packrat
, (<@>)
) where

import Control.Applicative ((*>))
import Text.Parsec.Prim

import Language.TinyBang.Display hiding ((</>))
import Language.TinyBang.Logging

$(loggingFunctions)

-- |A binary packrat parser operation.  The first parser is attempted; if it
--  fails, it consumes no input and the second parser is used instead.
packrat :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
packrat a b = try a <|> b

-- |An infix alias for @packrat@.
(</>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
(</>) = packrat
infixl 1 </>

-- |Wraps a parser in a debug logger.  This logger will log messages when the
--  parser starts, succeeds, or fails.  The first argument to this function is
--  a description of the parser in question.
loggingParser :: (Monad m, Display a)
              => String -> ParsecT s u m a -> ParsecT s u m a
loggingParser desc p = do
  st <- getParserState
  _debug $ "Trying " ++ desc ++ " at "  ++ show (statePos st)
  result <- try p
              <|> _debug ("Failed to parse " ++ desc ++ " at "
                      ++ show (statePos st))
                  *> parserZero
  _debug $ "Parsed " ++ desc ++ " at " ++ show (statePos st) ++ ": "
              ++ display result
  return result
  
-- |An infix synonym for @loggingParser@.
(<@>) :: (Monad m, Display a) => String -> ParsecT s u m a -> ParsecT s u m a
(<@>) = loggingParser
infixl 0 <@>
