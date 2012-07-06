module Language.LittleBang.Test.OnionSubtraction
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Interpreter.Ast as IA
import qualified Language.TinyBang.Config as Cfg
import Data.ExtensibleVariant

import qualified Data.IntMap as IntMap

zero :: TA.Value IA.Expr
zero = TA.VPrimInt 0

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Onion subtraction tests" $ TestList
  [ xvEval "`A 0 &- `A"
          TA.VEmptyOnion
  , xsEval "(`A 0 &- `A) & `A 0"
          ( TA.VLabel (labelName "A") $ 0
          , IntMap.fromList $ [(0,TA.VPrimInt 0)] )
  , xvEval "0 &- int"
          TA.VEmptyOnion
  , xvEval "0 &- char"
          zero
  , xvEval "(() & `A 0 & `A ()) &- `A"
          TA.VPrimUnit
  , xvEval "(`A 0 & `A ()) &- `A"
          TA.VEmptyOnion
  , xvEval "`A 0 & `A () & `A `B () &- `A & 0"
          zero
  , xvEval "(((`A 0 & `A ()) & `A `B ()) &- `A) & 0"
          zero

  -- Test that subtraction parses correctly
  , xPars "`A 0 & `A () & `A `B () &- `A & 0"
          (inj $ LA.Onion (
            (inj $ TA.OnionSub (
              (inj $ LA.Onion (
                (inj $ LA.Onion (
                  inj $ TA.Label (labelName "A") Nothing $
                    inj $ TA.PrimInt 0
                ) (inj $ TA.Label (labelName "A") Nothing $
                    inj $ TA.PrimUnit))
              ) (inj $ TA.Label (labelName "A") Nothing $
                   inj $ TA.Label (labelName "B") Nothing $
                     inj TA.PrimUnit))
            ) (TA.ProjLabel $ labelName "A"))
          ) (inj $ TA.PrimInt 0))

  , xPars "(((`A 0 & `A ()) & `A `B ()) &- `A) & 0"
          (inj $ LA.Onion (
            (inj $ TA.OnionSub (
              (inj $ LA.Onion (
                (inj $ LA.Onion (
                  inj $ TA.Label (labelName "A") Nothing $
                    inj $ TA.PrimInt 0
                ) (inj $ TA.Label (labelName "A") Nothing $
                    inj $ TA.PrimUnit))
              ) (inj $ TA.Label (labelName "A") Nothing
                 (inj $ TA.Label (labelName "B") Nothing $
                    inj $ TA.PrimUnit)))
            ) (TA.ProjLabel $ labelName "A"))
          ) (inj $ TA.PrimInt 0))
  ]