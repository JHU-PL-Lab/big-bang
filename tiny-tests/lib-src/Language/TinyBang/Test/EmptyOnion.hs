module Language.TinyBang.Test.EmptyOnion
( tests
)
where

import Language.TinyBang.Test.UtilFunctions

import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import Utils.Language.Ast

-- TODO: Write quickcheck properties that the empty onion is the left and right identity.

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests about the empty onion" $ TestList
  [ lexParseEval "(&)"
                 [TokOpenParen, TokOnionCons, TokCloseParen]
                 (astwrap $ A.EmptyOnion :: A.Expr)
                 (A.VEmptyOnion :: A.Value A.Expr)
  ]
