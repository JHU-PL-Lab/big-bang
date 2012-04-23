module Language.LittleBang.Test.Self
( tests
)
where

import Language.LittleBang.Test.NameUtils
import qualified Language.LittleBang.Test.ValueUtils as V
import Language.LittleBang.Test.UtilFunctions

import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg

import qualified Data.IntMap as IntMap

-- TODO: Write quickcheck properties that the empty onion is the left and right identity.

mkState = IntMap.fromList

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests of self" $ TestList
  [ xEval "def x = `Sum (fun n -> case n == 0 of {                      \
          \                         `True _ -> 0;                       \
          \                         `False _ -> n + (self.Sum (n - 1))  \
          \              }) in                                          \
          \x.Sum 5                                                      "
          $ V.pi 15
  , xEval "def evenOdd = `isEven (fun x -> case x of {                  \
          \                                  `S y -> self.isOdd y;      \
          \                                  `Z _ -> `True ()}) &       \
          \              `isOdd (fun x -> case x of {                   \
          \                                 `S y -> self.isEven y;      \
          \                                 `Z _ -> `False ()})         \
          \in evenOdd.isEven (`S `S `S `S `S `Z ())                     "
          $ ( TA.VLabel tlblFalse 0
            , mkState [ (0, TA.VPrimUnit) ]
            )
  ]
