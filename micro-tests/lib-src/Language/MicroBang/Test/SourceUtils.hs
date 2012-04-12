module Language.MicroBang.Test.SourceUtils
( srcY
, srcMultiAppl
, srcSummate
, srcGreaterOrLess
, lblEq
, lblLt
, lblGt
)
where

import qualified Language.MicroBang.Ast as A
import Language.MicroBang.Test.UtilFunctions

srcY  :: MicroBangCode
srcY  = "(fun body ->"
        ++ " (fun f -> fun arg -> f f arg)"
        ++ " (fun this -> fun arg -> body (this this) arg))"

srcMultiAppl :: [MicroBangCode] -> MicroBangCode
srcMultiAppl [] = error "srcMultiAppl used on empty list"
srcMultiAppl xs = concatMap (\x -> "(" ++ x ++ ")") xs

srcSummate :: MicroBangCode
srcSummate = "fun this -> fun x -> case ([=] x 0) of { `True z -> 0 ; `False z -> [+] x (this ([+] x -1))}"

srcGreaterOrLessUtil :: MicroBangCode
srcGreaterOrLessUtil =
 "fun this -> fun x -> fun y -> fun z ->"++
     "case [=] x ([+] y z) of {"++
          "`True junk -> `GreaterThan () ;"++
          "`False junk ->"++
                 "case [=] y ([+] x z) of {"++
                      "`True junk -> `LessThan () ;"++
                      "`False junk -> this x y ([+] z 1) }}"

srcGreaterOrLess :: MicroBangCode
srcGreaterOrLess =
 "fun x -> fun y ->"++
     "case [=] x y of {"++
          "`True junk -> `EqualTo () ;"++
           "`False junk -> "
           ++ srcMultiAppl [srcY, srcGreaterOrLessUtil, "x", "y", "1"]
           ++ "}"

lblEq, lblLt, lblGt :: Result
lblEq = (A.VLabel (labelName "EqualTo") $ A.VPrimUnit)
lblLt = (A.VLabel (labelName "LessThan") $ A.VPrimUnit)
lblGt = (A.VLabel (labelName "GreaterThan") $ A.VPrimUnit)