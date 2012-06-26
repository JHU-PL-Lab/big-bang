module Language.TinyBang.Test.Peano
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.SourceUtils
  ( srcY
  , tbDef
  , tbCase
  , srcMultiAppl
  )
import Language.TinyBang.Test.NameUtils
  ( lblZ
  , lblS
  )
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Data.IntMap (IntMap)

peanoSrcZero = tbDef "zero" "`Z ()"
peanoSrcTwo = tbDef "two" "`S `S `Z ()"
peanoSrcSucc = tbDef "succ" "x -> `S x"
peanoSrcY = tbDef "Y" srcY
peanoSrcPlus = tbDef "plus" (srcMultiAppl
                           [ "Y"
                           , "this -> x -> y ->" ++
                                tbCase "x" ["`Z _ -> y"
                                           ,"`S z -> this z (succ y)" ]])
peanoSrcMult =
  tbDef "multHelper" (srcMultiAppl
                      [ "Y"
                      , "this -> accum -> x -> y ->" ++
                          tbCase "x" ["`Z _ -> accum"
                                     ,"`S z -> this (plus accum y) z y"]]) .
  tbDef "mult" "multHelper zero"
--peanoSrcZero =
--  "def zero = `Z () in                                                         "
--peanoSrcTwo =
--  "def two = `S `S `Z () in                                                    "
--peanoSrcSucc =
--  "def succ = x -> `S x in                                                     "
--peanoSrcY =
--  "def Y = " ++ srcY ++ " in                                                   "
--peanoSrcPlus =
--  "def plus = Y (    this ->     x ->     y ->                                 \
--  \                case x of {                                                 \
--  \                    `Z x' -> y ;                                            \
--  \                    `S x' -> this x' (succ y) }) in                         "
--peanoSrcMult =
--  "def multHelper = Y (    this ->     accum ->     x ->     y ->              \
--  \                        case x of {                                         \
--  \                            `Z x' -> accum ;                                \
--  \                            `S x' -> this (plus accum y) x' y }) in         \
--  \def mult = multHelper zero in                                               "

peanoVal :: Int -> (A.Value IA.Expr, IntMap (A.Value IA.Expr))
peanoVal x = case x of
  0 -> (A.VLabel lblZ 0, makeState [(0,A.VPrimUnit)])
  1 -> (A.VLabel lblS 0, makeState [(0,A.VLabel lblZ 1), (1,A.VPrimUnit)])
  _ | x >= 0 ->
       (A.VLabel lblS x,
        makeState $ [(0, A.VPrimUnit), (1, A.VLabel lblZ 0)] ++
                    map (\n -> (n,A.VLabel lblS (n-1))) [2..x])
  _ -> error $ "Peano value requested for negative value: " ++ show x

peanoPrelude =
  peanoSrcZero . peanoSrcTwo . peanoSrcSucc . peanoSrcY . peanoSrcPlus

peanoPreludeMult = peanoPrelude . peanoSrcMult

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Peano tests" $ TestList
  [ xEval ( peanoPrelude "plus two two" ) $ peanoVal 4
  , xEval ( peanoPreludeMult "mult two two" ) $ peanoVal 4
  , xEval ( peanoPreludeMult $ tbDef "four" "plus two two" "mult four two")
          $ peanoVal 8
  ]
