module Language.LittleBang.Test.Self
( tests
)
where

import qualified Language.LittleBang.Test.ValueUtils as V
import Language.LittleBang.Test.UtilFunctions

import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg

-- TODO: Write quickcheck properties that the empty onion is the left and right identity.

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests of self" $ TestList
  [ xEval "def x = `Sum (fun n -> case n == 0 of {                      \
          \                         `True _ -> 0;                       \
          \                         `False _ -> n + (self.Sum (n - 1))  \
          \              }) in                                          \
          \x.Sum 5                                                      "
          $ V.pi 15
  ]
