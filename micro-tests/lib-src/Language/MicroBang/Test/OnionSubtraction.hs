module Language.MicroBang.Test.OnionSubtraction
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Ast as A

import qualified Data.IntMap as IntMap

zero = A.VPrimInt 0

tests :: (?conf :: Bool) => Test
tests = TestLabel "Onion subtraction tests" $ TestList
  [ xEval "`A 0 & -`A"
          A.VEmptyOnion
  , xEval "(`A 0 & -`A) & `A 0"
          ( A.VLabel (labelName "A") $ A.VPrimInt 0 )
  , xEval "0 & -int"
          A.VEmptyOnion
  , xEval "(() & `A 0 & `A ()) & -`A"
          A.VPrimUnit
  , xEval "(`A 0 & `A ()) & -`A"
          A.VEmptyOnion
  , xEval "`A 0 & `A () & `A `B () & -`A & 0"
          zero
  , xEval "(((`A 0 & `A ()) & `A `B ()) & -`A) & 0"
          zero
  ]