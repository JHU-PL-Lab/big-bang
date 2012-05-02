module Language.LittleBang.Test.Primitive.Unit
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg
import Utils.Language.Ast

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests for unit" $ TestList
  [ lexParseEval "()"
                 [TokOpenParen, TokCloseParen]
                 (astwrap TA.PrimUnit :: LA.Expr)
                 (TA.VPrimUnit :: TA.Value TA.Expr)
  , lexParseEval "( )"
                 [TokOpenParen, TokCloseParen]
                 (astwrap TA.PrimUnit :: LA.Expr)
                 (TA.VPrimUnit :: TA.Value TA.Expr)
  , xvEval "case () of { unit -> () }"
          TA.VPrimUnit
  ]
