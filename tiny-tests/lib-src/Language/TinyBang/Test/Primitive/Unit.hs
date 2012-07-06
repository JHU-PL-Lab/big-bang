module Language.TinyBang.Test.Primitive.Unit
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Data.ExtensibleVariant

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests for unit" $ TestList
  [ lexParseEval "()"
                 [TokOpenParen, TokCloseParen]
                 (inj A.PrimUnit)
                 (A.VPrimUnit :: A.Value IA.Expr)
  , lexParseEval "( )"
                 [TokOpenParen, TokCloseParen]
                 (inj A.PrimUnit)
                 (A.VPrimUnit :: A.Value IA.Expr)
  , xvEval "(unit -> ()) () "
          A.VPrimUnit
  ]