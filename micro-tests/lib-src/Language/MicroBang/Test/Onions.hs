module Language.MicroBang.Test.Onions
( tests
) where

import Language.MicroBang.Test.UtilFunctions
import Language.MicroBang.Test.ExpressionUtils as E
import Language.MicroBang.Test.ValueUtils as V
import Language.MicroBang.Test.NameUtils
  ( lblA
  , lblB
  , lblC
  , idX
  )

import qualified Language.MicroBang.Ast as A

import qualified Data.IntMap as IntMap


mkState = IntMap.fromList

-- TODO: write quickcheck test to generate a permutable onion and verify that
-- its permutations are equivalent.

tests :: (?conf :: Bool) => Test
tests = TestLabel "Tests of basic onion properties" $ TestList
  [ lexParseEval "`A 1 & `B 2"
                 ( A.Onion (A.Label lblA $ E.pi 1) (A.Label lblB $ E.pi 2) )
                 ( A.VOnion (A.VLabel lblA $ A.VPrimInt 1) (A.VLabel lblB $ A.VPrimInt 2))
  -- Test that right wins
  , xEval "`A 1 & `A 2"
          (A.VOnion (A.VLabel lblA $ A.VPrimInt 1) (A.VLabel lblA $ A.VPrimInt 2))

  ---- Test some parsing
  , xPars "(1 & (() & (fun x -> x)))"
          (A.Onion (A.PrimInt 1)
                   (A.Onion (A.PrimUnit)
                   (A.Func idX varX)))

  -- Test that onions associate right
  , xPars "`A 1 & `B 2 & `C 3"
          (A.Onion (A.Onion (A.Label lblA $ E.pi 1) (A.Label lblB $ E.pi 2))
                   (A.Label lblC $ E.pi 3))
  , xPars "(`A 1 & `B 2) & `C 3"
          (A.Onion (A.Onion (A.Label lblA $ E.pi 1) (A.Label lblB $ E.pi 2))
                   (A.Label lblC $ E.pi 3))
  , xPars "`A 1 & (`B 2 & `C 3)"
          (A.Onion (A.Label lblA $ E.pi 1)
                   (A.Onion (A.Label lblB $ E.pi 2) (A.Label lblC $ E.pi 3)))
  ]