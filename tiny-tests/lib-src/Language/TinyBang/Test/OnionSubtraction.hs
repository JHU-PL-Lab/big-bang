module Language.TinyBang.Test.OnionSubtraction
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg

import qualified Data.IntMap as IntMap

zero = A.VPrimInt 0

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Onion subtraction tests" $ TestList
  [ xEval "`A 0 &- `A"
          A.VEmptyOnion
  , xEval "(`A 0 &- `A) & `A 0"
          ( A.VLabel (labelName "A") $ 0
          , IntMap.fromList $ [(0,A.VPrimInt 0)] )
  , xEval "0 &- int"
          A.VEmptyOnion
  , xEval "0 &- char"
          zero
  , xEval "(() & `A 0 & `A ()) &- `A"
          A.VPrimUnit
  , xEval "(`A 0 & `A ()) &- `A"
          A.VEmptyOnion
  , xEval "`A 0 & `A () & `A `B () &- `A & 0"
          zero
  , xEval "(((`A 0 & `A ()) & `A `B ()) &- `A) & 0"
          zero

  -- Test that subtraction parses correctly
  , xPars "`A 0 & `A () & `A `B () &- `A & 0"
          (A.Onion (
            (A.OnionSub (
              (A.Onion (
                (A.Onion (
                  A.Label (labelName "A") Nothing $ A.PrimInt 0
                ) (A.Label (labelName "A") Nothing A.PrimUnit))
              ) (A.Label (labelName "A") Nothing
                 (A.Label (labelName "B") Nothing A.PrimUnit)))
            ) (A.ProjLabel $ labelName "A"))
          ) (A.PrimInt 0))

  , xPars "(((`A 0 & `A ()) & `A `B ()) &- `A) & 0"
          (A.Onion (
            (A.OnionSub (
              (A.Onion (
                (A.Onion (
                  A.Label (labelName "A") Nothing $ A.PrimInt 0
                ) (A.Label (labelName "A") Nothing A.PrimUnit))
              ) (A.Label (labelName "A") Nothing
                 (A.Label (labelName "B") Nothing A.PrimUnit)))
            ) (A.ProjLabel $ labelName "A"))
          ) (A.PrimInt 0))
  ]
