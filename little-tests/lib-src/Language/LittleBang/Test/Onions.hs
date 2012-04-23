module Language.LittleBang.Test.Onions
( tests
) where

import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.ExpressionUtils as E
import Language.LittleBang.Test.ValueUtils as V
import Language.LittleBang.Test.NameUtils
  ( tlblA
  , tlblB
  , llblA
  , llblB
  , llblC
  )

import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg

import qualified Data.IntMap as IntMap


mkState = IntMap.fromList

-- TODO: write quickcheck test to generate a permutable onion and verify that
-- its permutations are equivalent.

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Tests of basic onion properties" $ TestList
  [ lexParseEval "`A 1 & `B 2"
                 [ TokLabelPrefix
                 , TokIdentifier "A"
                 , TokIntegerLiteral 1
                 , TokOnionCons
                 , TokLabelPrefix
                 , TokIdentifier "B"
                 , TokIntegerLiteral 2
                 ]
                 ( LA.Onion (LA.Label llblA Nothing $ E.pi 1)
                           (LA.Label llblB Nothing $ E.pi 2) )
                 ( TA.VOnion (TA.VLabel tlblA 0) (TA.VLabel tlblB 1)
                 , mkState [(0, V.pi 1), (1, V.pi 2)]
                 )
  -- Test that right wins
  , xEval "`A 1 & `A 2"
          (TA.VLabel tlblA 0, mkState [(0, V.pi 2)])

  -- Test some parsing
  , xPars "(1 & ('x' & (fun x -> x)))"
          (LA.Onion (LA.PrimInt 1)
                   (LA.Onion (LA.PrimChar 'x')
                   E.identFuncX))

  -- Test that onions associate right
  , xPars "`A 1 & `B 2 & `C 3"
          (LA.Onion (LA.Onion (LA.Label llblA Nothing $ E.pi 1)
                            (LA.Label llblB Nothing $ E.pi 2))
                   (LA.Label llblC Nothing $ E.pi 3))
  , xPars "(`A 1 & `B 2) & `C 3"
          (LA.Onion (LA.Onion (LA.Label llblA Nothing $ E.pi 1)
                            (LA.Label llblB Nothing $ E.pi 2))
                   (LA.Label llblC Nothing $ E.pi 3))
  , xPars "`A 1 & (`B 2 & `C 3)"
          (LA.Onion (LA.Label llblA Nothing $ E.pi 1)
                   (LA.Onion (LA.Label llblB Nothing $ E.pi 2)
                            (LA.Label llblC Nothing $ E.pi 3)))
  -- Test that onions evaluate left to right
  -- Uses state
  , xEval " def x = 0 in \
          \ `A (x = x + 1 in x) & `B (x = x + 1 in x)"
          ( TA.VOnion (TA.VLabel tlblA 0) (TA.VLabel tlblB 1)
          , mkState [ (0, V.pi 1)
                    , (1, V.pi 2)
                    ]
          )
  , xEval " def x = `Ref 0 in \
          \ def inc = fun x -> case x of { `Ref y -> y = y + 1 in y } in \
          \ `A (inc x) & `B (inc x)"
          ( TA.VOnion (TA.VLabel tlblA 0) (TA.VLabel tlblB 1)
          , mkState [ (0, V.pi 1)
                    , (1, V.pi 2)
                    ]
          )
  , xEval " def r = `Ref 0 in \
          \ def inc = fun x -> case x of { `Ref y -> y = y + 1 in y } in \
          \ `A (case r of { `Ref x -> x = x + 1 in x }) & \
          \ `B (case r of { `Ref x -> x = x + 1 in x })"
          ( TA.VOnion (TA.VLabel tlblA 0) (TA.VLabel tlblB 1)
          , mkState [ (0, V.pi 1)
                    , (1, V.pi 2)
                    ]
          )
  -- Verify that prior is correctly bound
  , xEval "5 & `A prior"
          ( TA.VOnion (V.pi 5) $ TA.VLabel tlblA 0
          , mkState [ (0, V.pi 5) ]
          )
  , xEval "prior & prior"
          ( TA.VEmptyOnion )
  ]
