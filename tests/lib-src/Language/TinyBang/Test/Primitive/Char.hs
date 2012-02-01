module Language.TinyBang.Test.Primitive.Char
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A

-- TODO: write prop_testChar using quickcheck

testChar :: Char -> Test
testChar c = lexParseEval ('\'':c:'\'':[])
                          [TokCharLiteral c]
                          (A.PrimChar c)
                          (A.VPrimChar c)


tests = TestLabel "Character tests" $ TestList $
  map testChar
    ['a', 'A', 'z', 'Z', '∀', '∃', 'λ', '◈', '☺', '⤳', '☃', ' ', '\t']
--  TODO: We should do something about handling character escapse or literal newlines.
--  [ lexParseEval "'\\n'"
--                 [TokCharLiteral '\n']
--                 (A.PrimChar '\n')
--                 (A.VPrimChar '\n')
--  ]
