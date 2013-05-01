module Test.TinyBang.ValueDsl
( parseValueDslPredicate
, DeepOnionPredicate
) where

import Test.TinyBang.ValueDsl.Lexer
import Test.TinyBang.ValueDsl.Parser

-- | A function to parse a value DSL string into a predicate function.
parseValueDslPredicate :: String -> Either String DeepOnionPredicate
parseValueDslPredicate str = parseValueDsl =<< lexValueDsl str
