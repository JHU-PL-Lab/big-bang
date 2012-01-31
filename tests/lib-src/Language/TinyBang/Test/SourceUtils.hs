module Language.TinyBang.Test.SourceUtils
( srcY
, srcMultiAppl
)
where

import Language.TinyBang.Test.UtilFunctions (TinyBangCode)

srcY  :: TinyBangCode
srcY  = "fun body ->"
        ++ " (fun f -> fun arg -> f f arg)"
        ++ " (fun this -> fun arg -> body (this this) arg)"

srcMultiAppl :: [TinyBangCode] -> TinyBangCode
srcMultiAppl [] = error "srcMultiAppl used on empty list"
srcMultiAppl xs = concatMap (\x -> "(" ++ x ++ ")") xs

{-
srcSummate :: TinyBangCode
srcSummate = "fun this -> fun x -> case ([=] x 0) of { `True z -> 0 ; `False z -> [+] x (this ([-] x 1))}"

srcGreaterOrLessUtil :: TinyBangCode
srcGreaterOrLessUtil =
 "fun this -> fun x -> fun y -> fun z ->"++
     "case equal ([-] x y) z of {"++
          "`True junk -> `GreaterThan () ;"++
          "`False junk ->"++
                 "case [=] (minus y x) z of {"++
                      "`True junk -> `LessThan () ;"++
                      "`False junk -> this x y ([+] z 1) }}"

srcGreaterOrLess :: TinyBangCode
srcGreaterOrLess =
 "fun x -> fun y ->"++
     "case equal x y of {"++
          "`True junk -> `EqualTo () ;"++
           "`False junk -> "
           ++ srcMultiAppl [srcY, srcGreaterOrLessUtil, "x", "y", "1"]
           ++ "}"

lblEq, lblLt, lblGt :: Result
lblEq = (A.VLabel (labelName "EqualTo") 0, IntMap.singleton 0 A.VPrimUnit)
lblLt = (A.VLabel (labelName "LessThan") 0, IntMap.singleton 0 A.VPrimUnit)
lblGt = (A.VLabel (labelName "GreaterThan") 0, IntMap.singleton 0 A.VPrimUnit)
-}
