{-# LANGUAGE TupleSections, TemplateHaskell #-}

{-|
  Parses expectations for TBN test files.  These expectations are comparatively
  simple (excepting that they include the TBN grammar).  As with the TinyBang
  expectations, they must appear on a single line in a comment.
  @
    expect ::= matchExpect | typeFailExpect
    matchExpect ::= "EXPECT-MATCH:" pattern
    typeFailExpect ::= "EXPECT-TYPEFAIL"
  @
-}
module Test.TinyBangNested.SourceFile.Expectation
( Expectation(..)
, parseExpectation
) where

import Data.Either.Combinators
import Data.List

import Language.TinyBang.Utils.Logger
import Language.TinyBang.Utils.Syntax.Location
import Language.TinyBangNested.Ast
import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBangNested.Syntax.Parser

$(loggingFunctions)

-- TODO: add an "EXPECT-EQUIV" for value equivalence checks
data Expectation
  = ExpectMatches Pattern String
  | ExpectTypeFail
      
-- |Parses an expectation from a TinyBangNested comment.  A @Left@ indicates
--  a parse error in an expectation line.  A @Right Nothing@ indicates that no
--  expectation line was found.
parseExpectation :: String -> Either (String, String) (Maybe Expectation)
parseExpectation src =
  let src' = trim src in
  let ans = find ((`isPrefixOf` src') . fst) parsers in
  case ans of
    Nothing -> Right Nothing
    Just (s, p) ->
      mapBoth (src',) Just $ p $ drop (length s) src'
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    parsers =
      [ ("# EXPECT-MATCH:", parseExpectMatch)
      , ("# EXPECT-TYPEFAIL", const $ Right ExpectTypeFail)
      ]
    parseExpectMatch :: String -> Either String Expectation
    parseExpectMatch str = do
      tokens <- lexTinyBangNested UnknownDocument str
      tpat <- parseTinyBangNestedPattern UnknownDocument tokens
      return $ ExpectMatches tpat str
