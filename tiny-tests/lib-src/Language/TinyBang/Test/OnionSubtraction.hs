module Language.TinyBang.Test.OnionSubtraction
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Utils.Language.Ast

import qualified Data.IntMap as IntMap

zero :: A.Value IA.Expr
zero = A.VPrimInt 0

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Onion subtraction tests" $ TestList
  [ xvEval "`A 0 &- `A"
          A.VEmptyOnion
  , xsEval "(`A 0 &- `A) & `A 0"
          ( A.VLabel (labelName "A") $ 0
          , IntMap.fromList $ [(0,A.VPrimInt 0)] )
  , xvEval "0 &- int"
          A.VEmptyOnion
  , xvEval "0 &- char"
          zero
  , xvEval "(() & `A 0 & `A ()) &- `A"
          A.VPrimUnit
  , xvEval "(`A 0 & `A ()) &- `A"
          A.VEmptyOnion
  , xvEval "`A 0 & `A () & `A `B () &- `A & 0"
          zero
  , xvEval "(((`A 0 & `A ()) & `A `B ()) &- `A) & 0"
          zero

  -- Test that subtraction parses correctly
  , xPars "`A 0 & `A () & `A `B () &- `A & 0"
          (astwrap $ A.Onion (
            (astwrap $ A.OnionSub (
              (astwrap $ A.Onion (
                (astwrap $ A.Onion (
                  astwrap $ A.Label (labelName "A") Nothing $
                    astwrap $ A.PrimInt 0
                ) (astwrap $ A.Label (labelName "A") Nothing $
                    astwrap $ A.PrimUnit))
              ) (astwrap $ A.Label (labelName "A") Nothing $
                   astwrap $ A.Label (labelName "B") Nothing $
                     astwrap A.PrimUnit))
            ) (A.ProjLabel $ labelName "A"))
          ) (astwrap $ A.PrimInt 0))

  , xPars "(((`A 0 & `A ()) & `A `B ()) &- `A) & 0"
          (astwrap $ A.Onion (
            (astwrap $ A.OnionSub (
              (astwrap $ A.Onion (
                (astwrap $ A.Onion (
                  astwrap $ A.Label (labelName "A") Nothing $
                    astwrap $ A.PrimInt 0
                ) (astwrap $ A.Label (labelName "A") Nothing $
                    astwrap $ A.PrimUnit))
              ) (astwrap $ A.Label (labelName "A") Nothing
                 (astwrap $ A.Label (labelName "B") Nothing $
                    astwrap $ A.PrimUnit)))
            ) (A.ProjLabel $ labelName "A"))
          ) (astwrap $ A.PrimInt 0))
  ]
