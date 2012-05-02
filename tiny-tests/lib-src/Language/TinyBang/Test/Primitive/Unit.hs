module Language.TinyBang.Test.Primitive.Unit
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests for unit" $ TestList
  [ lexParseEval "()"
                 [TokOpenParen, TokCloseParen]
                 A.PrimUnit
                 A.VPrimUnit
  , lexParseEval "( )"
                 [TokOpenParen, TokCloseParen]
                 A.PrimUnit
                 A.VPrimUnit
  , xEval "case () of { unit -> () }"
          A.VPrimUnit
  ]
