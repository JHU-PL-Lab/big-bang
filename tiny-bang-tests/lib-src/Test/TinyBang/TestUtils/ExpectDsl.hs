module Test.TinyBang.TestUtils.ExpectDsl
( parseExpectDslPredicate
, DeepOnionPredicate
, Expectation(..)
) where

import Test.TinyBang.TestUtils.ExpectDsl.Data
import Test.TinyBang.TestUtils.ExpectDsl.Lexer
import Test.TinyBang.TestUtils.ExpectDsl.Parser

-- | A function to parse a value DSL string into a predicate function.
parseExpectDslPredicate :: String -> Either String Expectation
parseExpectDslPredicate str = do
  tokens <- lexValueDsl str
  result <- parseValueDsl tokens
  return $ result str
