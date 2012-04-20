module Language.LittleBang.Test.OnionSubtraction
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg

import qualified Data.IntMap as IntMap

zero = TA.VPrimInt 0

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Onion subtraction tests" $ TestList
  [ xEval "`A 0 &- `A"
          TA.VEmptyOnion
  , xEval "(`A 0 &- `A) & `A 0"
          ( TA.VLabel (tLabelName "A") $ 0
          , IntMap.fromList $ [(0,TA.VPrimInt 0)] )
  , xEval "0 &- int"
          TA.VEmptyOnion
  , xEval "0 &- char"
          zero
  , xEval "(() & `A 0 & `A ()) &- `A"
          TA.VPrimUnit
  , xEval "(`A 0 & `A ()) &- `A"
          TA.VEmptyOnion
  , xEval "`A 0 & `A () & `A `B () &- `A & 0"
          zero
  , xEval "(((`A 0 & `A ()) & `A `B ()) &- `A) & 0"
          zero

  -- Test that subtraction parses correctly
  , xPars "`A 0 & `A () & `A `B () &- `A & 0"
          (LA.Onion (
            (LA.OnionSub (
              (LA.Onion (
                (LA.Onion (
                  LA.Label (lLabelName "A") Nothing $ LA.PrimInt 0
                ) (LA.Label (lLabelName "A") Nothing LA.PrimUnit))
              ) (LA.Label (lLabelName "A") Nothing
                 (LA.Label (lLabelName "B") Nothing LA.PrimUnit)))
            ) (LA.ProjLabel $ lLabelName "A"))
          ) (LA.PrimInt 0))

  , xPars "(((`A 0 & `A ()) & `A `B ()) &- `A) & 0"
          (LA.Onion (
            (LA.OnionSub (
              (LA.Onion (
                (LA.Onion (
                  LA.Label (lLabelName "A") Nothing $ LA.PrimInt 0
                ) (LA.Label (lLabelName "A") Nothing LA.PrimUnit))
              ) (LA.Label (lLabelName "A") Nothing
                 (LA.Label (lLabelName "B") Nothing LA.PrimUnit)))
            ) (LA.ProjLabel $ lLabelName "A"))
          ) (LA.PrimInt 0))
  ]
