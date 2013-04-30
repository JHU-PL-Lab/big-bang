module Tests.ValueDsl
( parseValueDslPredicate
, DeepOnionPredicate
) where

import Tests.ValueDsl.Lexer
import Tests.ValueDsl.Parser

-- | A function to parse a value DSL string into a predicate function.
parseValueDslPredicate :: String -> Either String DeepOnionPredicate
parseValueDslPredicate str = parseValueDsl =<< lexValueDsl str
