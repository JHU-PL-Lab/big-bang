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
srcY  = "(fun body ->"
        ++ " (fun f -> fun arg -> f f arg)"
        ++ " (fun this -> fun arg -> body (this this) arg))"

srcMultiAppl :: [LittleBangCode] -> LittleBangCode
srcMultiAppl [] = error "srcMultiAppl used on empty list"
srcMultiAppl xs = concatMap (\x -> "(" ++ x ++ ")") xs

srcSummate :: LittleBangCode
srcSummate = "fun this -> fun x -> case (x == 0) of { `True z -> 0 ; `False z -> x + (this (x - 1))}"

srcGreaterOrLessUtil :: LittleBangCode
srcGreaterOrLessUtil =
 "fun this -> fun x -> fun y -> fun z ->"++
     "case (x - y) == z of {"++
          "`True junk -> `GreaterThan () ;"++
          "`False junk ->"++
                 "case (y - x) == z of {"++
                      "`True junk -> `LessThan () ;"++
                      "`False junk -> this x y (z + 1) }}"

srcGreaterOrLess :: LittleBangCode
srcGreaterOrLess =
 "fun x -> fun y ->"++
     "case x == y of {"++
          "`True junk -> `EqualTo () ;"++
           "`False junk -> "
           ++ srcMultiAppl [srcY, srcGreaterOrLessUtil, "x", "y", "1"]
           ++ "}"

lblEq, lblLt, lblGt :: Result
lblEq = (TA.VLabel (tLabelName "EqualTo") 0, makeState [(0,TA.VPrimUnit)])
lblLt = (TA.VLabel (tLabelName "LessThan") 0, makeState [(0,TA.VPrimUnit)])
lblGt = (TA.VLabel (tLabelName "GreaterThan") 0, makeState [(0,TA.VPrimUnit)])
