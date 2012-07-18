module Language.LittleBang.Test.SourceUtils
( srcY
, srcMultiAppl
, srcSummate
, srcGreaterOrLess
, lblEq
, lblLt
, lblGt
)
where

import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import Language.LittleBang.Test.UtilFunctions

srcY  :: LittleBangCode
srcY  = "(body ->"
        ++ " (f -> arg -> f f arg)"
        ++ " (this -> arg -> body (this this) arg))"

srcMultiAppl :: [LittleBangCode] -> LittleBangCode
srcMultiAppl [] = error "srcMultiAppl used on empty list"
srcMultiAppl xs = concatMap (\x -> "(" ++ x ++ ")") xs

srcSummate :: LittleBangCode
srcSummate = "this -> x -> "++
                "((`True z -> 0) &"++
                " (`False z -> x + (this (x - 1))))"++
                "(x == 0)"

srcGreaterOrLessUtil :: LittleBangCode
srcGreaterOrLessUtil =
 "this -> x -> y -> z ->"++
     "((`True junk -> `GreaterThan ()) &"++
     " (`False junk ->"++
        "((`True junk -> `LessThan ()) &"++
        " (`False junk -> this x y (z + 1)))"++
        "((y - x) == z)"++
     " ))"++
     "((x - y) == z)"

srcGreaterOrLess :: LittleBangCode
srcGreaterOrLess =
 "x -> y ->"++
     "((`True junk -> `EqualTo ()) &"++
     " (`False junk -> "
        ++ srcMultiAppl [srcY, srcGreaterOrLessUtil, "x", "y", "1"] ++
     " ))" ++
     "(x == y)"

lblEq, lblLt, lblGt :: Result
lblEq = (TA.VLabel (labelName "EqualTo") 0, makeState [(0,TA.VPrimUnit)])
lblLt = (TA.VLabel (labelName "LessThan") 0, makeState [(0,TA.VPrimUnit)])
lblGt = (TA.VLabel (labelName "GreaterThan") 0, makeState [(0,TA.VPrimUnit)])
