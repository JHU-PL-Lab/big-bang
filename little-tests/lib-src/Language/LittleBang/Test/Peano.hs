module Language.LittleBang.Test.Peano
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.SourceUtils
import Language.LittleBang.Test.NameUtils
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg

peanoSrcZero =
  "def zero = `Z () in                                                         "
peanoSrcTwo =
  "def two = `S `S `Z () in                                                    "
peanoSrcSucc =
  "def succ = fun x -> `S x in                                                 "
peanoSrcY =
  "def Y = " ++ srcY ++ " in                                                   "
peanoSrcPlus =
  "def plus = Y (fun this -> fun x -> fun y ->                                 \
  \                case x of {                                                 \
  \                    `Z x' -> y ;                                            \
  \                    `S x' -> this x' (succ y) }) in                         "
peanoSrcMult =
  "def multHelper = Y (fun this -> fun accum -> fun x -> fun y ->              \
  \                        case x of {                                         \
  \                            `Z x' -> accum ;                                \
  \                            `S x' -> this (plus accum y) x' y }) in         \
  \def mult = multHelper zero in                                               "

peanoVal x = case x of
  0 -> (TA.VLabel tlblZ 0, makeState [(0,TA.VPrimUnit)])
  1 -> (TA.VLabel tlblS 0, makeState [(0,TA.VLabel tlblZ 1), (1,TA.VPrimUnit)])
  _ | x >= 0 ->
       (TA.VLabel tlblS x,
        makeState $ [(0, TA.VPrimUnit), (1, TA.VLabel tlblZ 0)] ++
                    map (\n -> (n,TA.VLabel tlblS (n-1))) [2..x])
  _ -> error $ "Peano value requested for negative value: " ++ show x

peanoPrelude =
  peanoSrcZero ++ peanoSrcTwo ++ peanoSrcSucc ++ peanoSrcY ++ peanoSrcPlus

peanoPreludeMult = peanoPrelude ++ peanoSrcMult

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Peano tests" $ TestList
  [ xEval ( peanoPrelude ++ "plus two two" ) $ peanoVal 4
  , xEval ( peanoPreludeMult ++ "mult two two" ) $ peanoVal 4
  , xEval ( peanoPreludeMult ++
           "def four = plus two two in mult four two") $ peanoVal 8
  ]
