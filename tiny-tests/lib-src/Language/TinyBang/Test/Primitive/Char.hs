module Language.TinyBang.Test.Primitive.Char
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import Utils.Language.Ast

-- TODO: write prop_testChar using quickcheck

testChar :: (?conf :: Cfg.Config) => Char -> Test
testChar c = lexParseEval ('\'':c:'\'':[])
                          [flip TokCharLiteral c]
                          (astwrap $ A.PrimChar c)
                          (A.VPrimChar c :: A.Value A.Expr)


tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Character tests" $ TestList $
  map testChar
    ['a', 'A', 'z', 'Z', '∀', '∃', 'λ', '◈', '☺', '⤳', '☃', ' ', '\t']
--  TODO: We should do something about handling character escapse or literal newlines.
--  [ lexParseEval "'\\n'"
--                 [TokCharLiteral '\n']
--                 (A.PrimChar '\n')
--                 (A.VPrimChar '\n')
--  ]
