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
parseExpectDslPredicate str = (`parseValueDsl` str) =<< lexValueDsl str
