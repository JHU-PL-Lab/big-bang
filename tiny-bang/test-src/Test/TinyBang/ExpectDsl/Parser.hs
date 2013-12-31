{-# LANGUAGE LambdaCase, ExistentialQuantification #-}

module Test.TinyBang.ExpectDsl.Parser
( parseValueDsl
, DeepOnionPredicate
) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Utils.Parser.Parsec
import Test.TinyBang.ExpectDsl.Lexer
import Test.TinyBang.ExpectDsl.Data

-- |A function to parse the value DSL's tokens into a predicate function which
--  matches against @DeepValue@ structures.  If this is successful, the result
--  is the right predicate function; otherwise, it is a left error message.
parseValueDsl :: [PositionalToken] -- ^ The tokenized source
              -> String -- ^ The original source string (for reference)
              -> Either String Expectation -- ^ The expectation or an error
parseValueDsl toks src = case parse (programParser src) "<input>" toks of
  Left err -> Left $ show err
  Right ans -> Right ans

-- |A type alias for the TinyBang parser.
type Parser a = Parsec [PositionalToken] () a

programParser :: String -> Parser Expectation
programParser src = expectationParser src <* eof

expectationParser :: String -> Parser Expectation
expectationParser src =
      Pass <$> predicateParser <*> pure src
  </> consume TokTypeFail *> pure TypeFailure

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
      fromMaybe False $ inner <$> Map.lookup n (deepLabels onion)

primitiveParser :: Parser DeepOnionPredicate
primitiveParser =
      intParser
  </> emptyOnionParser

intParser :: Parser DeepOnionPredicate
intParser = demandPrim PrimInt <$> require matchInt
  where
    matchInt = \case
      TokLitInt n -> Just $ VInt generated n
      _ -> Nothing
      
emptyOnionParser :: Parser DeepOnionPredicate
emptyOnionParser = consume TokEmptyOnion *> return (const True)
  
demandPrim :: PrimitiveType -> PrimitiveValue -> DeepOnionPredicate
demandPrim dpt val onion = fromMaybe False $
  (==) val <$> Map.lookup dpt (deepPrimitives onion)

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
