module Language.MicroBang.Test.Primitive.Char
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Ast as A

-- TODO: write prop_testChar using quickcheck

testChar :: (?debug :: Bool) => Char -> Test
testChar c = lexParseEval ('\'':c:'\'':[])
                          [TokCharLiteral c]
                          (A.PrimChar c)
                          (A.VPrimChar c)


tests :: (?debug :: Bool) => Test
tests = TestLabel "Character tests" $ TestList $
  map testChar
    ['a', 'A', 'z', 'Z', '∀', '∃', 'λ', '◈', '☺', '⤳', '☃', ' ', '\t']
--  TODO: We should do something about handling character escapse or literal newlines.
--  [ lexParseEval "'\\n'"
--                 [TokCharLiteral '\n']
--                 (A.PrimChar '\n')
--                 (A.VPrimChar '\n')
--  ]