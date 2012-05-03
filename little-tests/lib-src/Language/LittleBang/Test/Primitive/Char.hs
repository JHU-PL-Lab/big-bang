module Language.LittleBang.Test.Primitive.Char
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Utils.Language.Ast

-- TODO: write prop_testChar using quickcheck

testChar :: (?conf :: Cfg.Config) => Char -> Test
testChar c = lexParseEval ('\'':c:'\'':[])
                          [TokCharLiteral c]
                          (astwrap $ TA.PrimChar c :: LA.Expr)
                          (TA.VPrimChar c :: TA.Value IA.Expr)


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
