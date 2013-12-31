{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, ConstraintKinds #-}

{-|
  This module contains general utilities for building a parsec parser for
  data types which retain @DocumentPosition@ information.
-}
module Language.TinyBang.Utils.Parser.OriginParser
( origConstr1
, origConstr2
, origConstr3
, origConstr4
, originParser
) where

import Control.Monad.Trans.Class
import Text.Parsec

import Language.TinyBang.Ast.Origin
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Utils.Parser.PositionalParser

origConstr1 :: (PositionalParserConstraints s u m c t)
            => (Origin -> a1 -> r)
            -> ParsecT s u m a1 -> ParsecT s u m r
origConstr1 = _origConstr

origConstr2 :: (PositionalParserConstraints s u m c t)
            => (Origin -> a1 -> a2 -> r)
            -> ParsecT s u m (a1,a2)
            -> ParsecT s u m r
origConstr2 constr = _origConstr (\o (a1,a2) -> constr o a1 a2)
  
origConstr3 :: (PositionalParserConstraints s u m c t)
            => (Origin -> a1 -> a2 -> a3 -> r)
            -> ParsecT s u m (a1,a2,a3)
            -> ParsecT s u m r
origConstr3 constr = _origConstr (\o (a1,a2,a3) -> constr o a1 a2 a3)
  
origConstr4 :: (PositionalParserConstraints s u m c t)
            => (Origin -> a1 -> a2 -> a3 -> a4 -> r)
            -> ParsecT s u m (a1,a2,a3,a4)
            -> ParsecT s u m r
origConstr4 constr = _origConstr (\o (a1,a2,a3,a4) -> constr o a1 a2 a3 a4)
  
_origConstr :: (PositionalParserConstraints s u m c t)
            => (Origin -> x -> r)
            -> ParsecT s u m x
            -> ParsecT s u m r
_origConstr constr p = do
  (orig,a) <- originParser p
  return $ constr orig a

originParser :: (PositionalParserConstraints s u m c t)
             => ParsecT s u m a -> ParsecT s u m (Origin, a)
originParser p = do
  (r, mpos) <- positionParser p
  document <- lift parserSourceDocument
  let origSpan = maybe UnknownSpan (uncurry $ DocumentSpan document) mpos
  return (SourceOrigin origSpan, r)

