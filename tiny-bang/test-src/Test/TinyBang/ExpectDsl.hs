module Test.TinyBang.ExpectDsl
( parseExpectDslPredicate
, DeepOnionPredicate
, Expectation(..)
) where

import Test.TinyBang.ExpectDsl.Data
import Test.TinyBang.ExpectDsl.Lexer
import Test.TinyBang.ExpectDsl.Parser

-- | A function to parse a value DSL string into a predicate function.
parseExpectDslPredicate :: String -> Either String Expectation
parseExpectDslPredicate str = (`parseValueDsl` str) =<< lexValueDsl str
