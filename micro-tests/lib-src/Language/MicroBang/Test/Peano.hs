module Language.MicroBang.Test.Peano
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import Language.MicroBang.Test.SourceUtils
  ( srcY
  )
import Language.MicroBang.Test.NameUtils
  ( lblZ
  , lblS
  )
import qualified Language.MicroBang.Ast as A

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
  0 -> (A.VLabel lblZ 0, makeState [(0,A.VPrimUnit)])
  1 -> (A.VLabel lblS 0, makeState [(0,A.VLabel lblZ 1), (1,A.VPrimUnit)])
  _ | x >= 0 ->
       (A.VLabel lblS x,
        makeState $ [(0, A.VPrimUnit), (1, A.VLabel lblZ 0)] ++
                    map (\n -> (n,A.VLabel lblS (n-1))) [2..x])
  _ -> error $ "Peano value requested for negative value: " ++ show x

peanoPrelude =
  peanoSrcZero ++ peanoSrcTwo ++ peanoSrcSucc ++ peanoSrcY ++ peanoSrcPlus

peanoPreludeMult = peanoPrelude ++ peanoSrcMult

tests :: (?debug :: Bool) => Test
tests = TestLabel "Peano tests" $ TestList
  [ xEval ( peanoPrelude ++ "plus two two" ) $ peanoVal 4
  , xEval ( peanoPreludeMult ++ "mult two two" ) $ peanoVal 4
  , xEval ( peanoPreludeMult ++
           "def four = plus two two in mult four two") $ peanoVal 8
  ]