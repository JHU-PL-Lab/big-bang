module Language.LittleBang.Test.EmptyOnion
( tests
)
where

import Language.LittleBang.Test.UtilFunctions

import qualified Language.TinyBang.Ast as TA
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Config as Cfg
import Utils.Language.Ast

-- TODO: Write quickcheck properties that the empty onion is the left and right identity.

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests about the empty onion" $ TestList
  [ lexParseEval "(&)"
                 [TokOpenParen, TokOnionCons, TokCloseParen]
                 (astwrap $ TA.EmptyOnion)
                 (TA.VEmptyOnion :: TA.Value TA.Expr)
  ]
