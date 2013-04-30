{-# LANGUAGE RankNTypes, LambdaCase #-}

module Tests.ValueDsl.Parser
( parseValueDsl
, DeepOnionPredicate
) where

import Prelude hiding (lookup)

import Control.Applicative           ((*>), (<$>), (<*))
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Utils.Parsec
import Tests.ValueDsl.Lexer

-- |A function to parse the value DSL's tokens into a predicate function which
--  matches against @DeepValue@ structures.  If this is successful, the result
--  is the right predicate function; otherwise, it is a left error message.
parseValueDsl :: [PositionalToken] -> Either String DeepOnionPredicate
parseValueDsl toks = case parse programParser "<input>" toks of
  Left err -> Left $ show err
  Right ans -> Right ans

-- |The type of predicate function produced by this module.
type DeepOnionPredicate = DeepOnion -> Bool

-- |A type alias for the TinyBang parser.
type Parser a = Parsec [PositionalToken] () a

programParser :: Parser DeepOnionPredicate
programParser = predicateParser <* eof

predicateParser :: Parser DeepOnionPredicate
predicateParser =
      onionParser
  </> primaryParser

onionParser :: Parser DeepOnionPredicate
onionParser = do
  left <- primaryParser
  consume TokOnion
  right <- predicateParser
  return $ \onion -> left onion && right onion

primaryParser :: Parser DeepOnionPredicate
primaryParser =
      labelParser
  </> primitiveParser
  </> consume TokOpenParen *> predicateParser <* consume TokCloseParen

labelParser :: Parser DeepOnionPredicate
labelParser = do
  name <- require matchLabelName
  inner <- primaryParser
  return $ demandLabel name inner
  where
    matchLabelName = \case
      TokLabel n -> Just $ LabelName (ComputedOrigin []) n
      _ -> Nothing
    demandLabel :: LabelName -> DeepOnionPredicate -> DeepOnionPredicate
    demandLabel n inner onion =
      fromMaybe False $ inner <$> Map.lookup n (deepOnionLabels onion)

primitiveParser :: Parser DeepOnionPredicate
primitiveParser =
      intParser
  </> charParser

intParser :: Parser DeepOnionPredicate
intParser = demandPrim DeepPrimInt <$> require matchInt
  where
    matchInt = \case
      TokLitInt n -> Just n
      _ -> Nothing
      
charParser :: Parser DeepOnionPredicate
charParser = demandPrim DeepPrimChar <$> require matchChar
  where
    matchChar = \case
      TokLitChar c -> Just c
      _ -> Nothing
  
demandPrim :: (Eq a) => DeepPrimitiveType a -> a -> DeepOnionPredicate
demandPrim dpt val onion = fromMaybe False $
  (==) val <$> lookup dpt (deepOnionPrimitives onion)

-- |This parser is a specialization of the @token@ parser with the necessary
--  pretty-printing and location-calculating routines embedded.
require :: forall a. (Token -> Maybe a) -> Parser a
require f = do
  tokenPrim show nextPos (f . gettok)
  where
    nextPos :: SourcePos
            -> PositionalToken
            -> [PositionalToken]
            -> SourcePos
    nextPos _ tok toks =
      case runIdentity (uncons toks) of
        Nothing -> getpos tok
        Just (tok',_) -> getpos tok'
    getpos :: PositionalToken -> SourcePos
    getpos (PositionalToken sp _) = sp
    gettok :: PositionalToken -> Token
    gettok (PositionalToken _ t) = t

-- |This parser is a specialization of @require@ which demands exact token
--  equality.  If it is matched, the second argument is returned.
requirex :: Token -> a -> Parser a
requirex t v = do
  _ <- require $ \t' -> if t == t' then Just () else Nothing
  return v
  
-- |This parser consumes a single token, producing nothing.
consume :: Token -> Parser ()
consume tok = requirex tok ()
