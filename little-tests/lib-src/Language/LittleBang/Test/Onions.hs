module Language.LittleBang.Test.Onions
( tests
) where

import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.ExpressionUtils as E
import Language.LittleBang.Test.ValueUtils as V
import Language.LittleBang.Test.NameUtils
  ( lblA
  , lblB
  , lblC
  , idX
  )

import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Interpreter.Ast as IA
import qualified Language.TinyBang.Config as Cfg
import Data.ExtensibleVariant

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
                 ( LA.onion
                    (TA.label lblA Nothing $ E.pi 1)
                    (TA.label lblB Nothing $ E.pi 2)
                    :: LA.Expr )
                 ( TA.VOnion (TA.VLabel lblA 0) (TA.VLabel lblB 1)
                    :: TA.Value IA.Expr
                 , mkState [(0, V.pi 1), (1, V.pi 2)]
                 )
  -- Test that right wins
  , xsEval "`A 1 & `A 2"
          (TA.VLabel lblA 0, mkState [(0, V.pi 2)])

  -- Test some parsing
  , xPars "(1 & ('x' & (x -> x)))"
          (LA.onion
            (TA.primInt 1)
            (LA.onion
              (TA.primChar 'x')
              (LA.scape (E.simplePat idX) varX)))

  -- Test that onions associate right
  , xPars "`A 1 & `B 2 & `C 3"
          (LA.onion
            (LA.onion
              (TA.label lblA Nothing $ E.pi 1)
              (TA.label lblB Nothing $ E.pi 2))
            (TA.label lblC Nothing $ E.pi 3))
  , xPars "(`A 1 & `B 2) & `C 3"
          (LA.onion
            (LA.onion
              (TA.label lblA Nothing $ E.pi 1)
              (TA.label lblB Nothing $ E.pi 2))
            (TA.label lblC Nothing $ E.pi 3))
  , xPars "`A 1 & (`B 2 & `C 3)"
          (LA.onion
            (TA.label lblA Nothing $ E.pi 1)
            (LA.onion
              (TA.label lblB Nothing $ E.pi 2)
              (TA.label lblC Nothing $ E.pi 3)))
  -- Test that onions evaluate left to right
  -- Uses state
  , xsEval " def x = 0 in \
          \ `A (x = x + 1 in x) & `B (x = x + 1 in x)"
          ( TA.VOnion (TA.VLabel lblA 0) (TA.VLabel lblB 1)
          , mkState [ (0, V.pi 1)
                    , (1, V.pi 2)
                    ]
          )
  , xsEval " def x = `Ref 0 in \
          \ def inc = x -> (`Ref y -> y = y + 1 in y) x in \
          \ `A (inc x) & `B (inc x)"
          ( TA.VOnion (TA.VLabel lblA 0) (TA.VLabel lblB 1)
          , mkState [ (0, V.pi 1)
                    , (1, V.pi 2)
                    ]
          )
  , xsEval " def r = `Ref 0 in                             \
          \ def inc = x -> (`Ref y -> y = y + 1 in y) x in \
          \ `A ( (`Ref x -> x = x + 1 in x) r) &           \
          \ `B ( (`Ref x -> x = x + 1 in x) r)             "
          ( TA.VOnion (TA.VLabel lblA 0) (TA.VLabel lblB 1)
          , mkState [ (0, V.pi 1)
                    , (1, V.pi 2)
                    ]
          )
  ]
