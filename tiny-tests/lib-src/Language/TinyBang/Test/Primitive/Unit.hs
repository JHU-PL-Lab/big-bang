module Language.TinyBang.Test.Primitive.Unit
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import Utils.Language.Ast

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests for unit" $ TestList
  [ lexParseEval "()"
                 [TokOpenParen, TokCloseParen]
                 (astwrap A.PrimUnit)
                 (A.VPrimUnit :: A.Value A.Expr)
  , lexParseEval "( )"
                 [TokOpenParen, TokCloseParen]
                 (astwrap A.PrimUnit)
                 (A.VPrimUnit :: A.Value A.Expr)
  , xvEval "case () of { unit -> () }"
          A.VPrimUnit
  ]
