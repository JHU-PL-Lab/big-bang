module Test.PatBang.ExpectDsl
( parseExpectDslPredicate
, DeepOnionPredicate
, Expectation(..)
) where

import Test.PatBang.ExpectDsl.Data
import Test.PatBang.ExpectDsl.Lexer
import Test.PatBang.ExpectDsl.Parser

-- | A function to parse a value DSL string into a predicate function.
parseExpectDslPredicate :: String -> Either String Expectation
parseExpectDslPredicate str = (`parseValueDsl` str) =<< lexValueDsl str
