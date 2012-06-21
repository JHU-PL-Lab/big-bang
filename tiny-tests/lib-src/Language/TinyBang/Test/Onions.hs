module Language.TinyBang.Test.Onions
( tests
) where

import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.ExpressionUtils as E
import Language.TinyBang.Test.ValueUtils as V
import Language.TinyBang.Test.NameUtils
  ( lblA
  , lblB
  , lblC
  , idX
  )

import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Utils.Language.Ast

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
                 ( astwrap $ A.Onion
                    (astwrap $ A.Label lblA Nothing $ E.pi 1)
                    (astwrap $ A.Label lblB Nothing $ E.pi 2) )
                 ( A.VOnion (A.VLabel lblA 0) (A.VLabel lblB 1)
                    :: A.Value IA.Expr
                 , mkState [(0, V.pi 1), (1, V.pi 2)]
                 )
  -- Test that right wins
  , xsEval "`A 1 & `A 2"
          (A.VLabel lblA 0, mkState [(0, V.pi 2)])

  -- Test some parsing
  , xPars "(1 & ('x' & (fun x -> x)))"
          (astwrap $ A.Onion
            (astwrap $ A.PrimInt 1)
            (astwrap $ A.Onion
              (astwrap $ A.PrimChar 'x')
              (astwrap $ A.Scape (A.Pattern idX $ A.PatOnion []) varX)))

  -- Test that onions associate right
  , xPars "`A 1 & `B 2 & `C 3"
          (astwrap $ A.Onion
            (astwrap $ A.Onion
              (astwrap $ A.Label lblA Nothing $ E.pi 1)
              (astwrap $ A.Label lblB Nothing $ E.pi 2))
            (astwrap $ A.Label lblC Nothing $ E.pi 3))
  , xPars "(`A 1 & `B 2) & `C 3"
          (astwrap $ A.Onion
            (astwrap $ A.Onion
              (astwrap $ A.Label lblA Nothing $ E.pi 1)
              (astwrap $ A.Label lblB Nothing $ E.pi 2))
            (astwrap $ A.Label lblC Nothing $ E.pi 3))
  , xPars "`A 1 & (`B 2 & `C 3)"
          (astwrap $ A.Onion
            (astwrap $ A.Label lblA Nothing $ E.pi 1)
            (astwrap $ A.Onion
              (astwrap $ A.Label lblB Nothing $ E.pi 2)
              (astwrap $ A.Label lblC Nothing $ E.pi 3)))
  -- Test that onions evaluate left to right
  -- Uses state
  , xsEval " def x = 0 in \
          \ `A (x = x + 1 in x) & `B (x = x + 1 in x)"
          ( A.VOnion (A.VLabel lblA 0) (A.VLabel lblB 1)
          , mkState [ (0, V.pi 1)
                    , (1, V.pi 2)
                    ]
          )
  , xsEval " def x = `Ref 0 in \
          \ def inc = fun x -> case x of { `Ref y -> y = y + 1 in y } in \
          \ `A (inc x) & `B (inc x)"
          ( A.VOnion (A.VLabel lblA 0) (A.VLabel lblB 1)
          , mkState [ (0, V.pi 1)
                    , (1, V.pi 2)
                    ]
          )
  , xsEval " def r = `Ref 0 in \
          \ def inc = fun x -> case x of { `Ref y -> y = y + 1 in y } in \
          \ `A (case r of { `Ref x -> x = x + 1 in x }) & \
          \ `B (case r of { `Ref x -> x = x + 1 in x })"
          ( A.VOnion (A.VLabel lblA 0) (A.VLabel lblB 1)
          , mkState [ (0, V.pi 1)
                    , (1, V.pi 2)
                    ]
          )
  ]
