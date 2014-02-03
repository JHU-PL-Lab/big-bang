module Test.TestUtils.ExpectDsl
( parseExpectDslPredicate
, DeepOnionPredicate
, Expectation(..)
) where

import Test.TestUtils.ExpectDsl.Data
import Test.TestUtils.ExpectDsl.Lexer
import Test.TestUtils.ExpectDsl.Parser

-- | A function to parse a value DSL string into a predicate function.
parseExpectDslPredicate :: String -> Either String Expectation
parseExpectDslPredicate str = (`parseValueDsl` str) =<< lexValueDsl str
