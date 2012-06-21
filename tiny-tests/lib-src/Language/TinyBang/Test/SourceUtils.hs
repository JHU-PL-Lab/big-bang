module Language.TinyBang.Test.SourceUtils
( srcY
, srcMultiAppl
, srcSummate
, srcGreaterOrLess
, lblEq
, lblLt
, lblGt
, tbCase
, tbDef
, tbDefInc
, tbLet
)
where

import qualified Language.TinyBang.Ast as A
import Language.TinyBang.Test.UtilFunctions
import Text.Printf (printf)
import Data.List (intercalate, intersperse)

parens :: String -> String
parens = printf "(%s)"

tbCase :: TinyBangCode -> [TinyBangCode] -> TinyBangCode
tbCase e bs =
  (++ (" " ++ parens e)) $ intercalate " & " $ reverse $ map parens bs

-- TODO: make this not use def
tbDef :: TinyBangCode -> TinyBangCode -> TinyBangCode -> TinyBangCode
tbDef defWhat eqWhat inWhat =
  printf "(`Ref %s -> (%s)) (`Ref (%s))" defWhat inWhat eqWhat

tbLet :: TinyBangCode -> TinyBangCode -> TinyBangCode -> TinyBangCode
tbLet letWhat eqWhat inWhat =
  printf "(%s -> (%s)) (%s)" letWhat inWhat eqWhat

tbDefInc :: TinyBangCode -> TinyBangCode
tbDefInc = tbDef "inc" "`Ref y -> y = y + 1 in y"

srcY  :: TinyBangCode
srcY  = "(body ->"
        ++ " (f -> arg -> f f arg)"
        ++ " (this -> arg -> body (this this) arg))"

srcMultiAppl :: [TinyBangCode] -> TinyBangCode
srcMultiAppl [] = error "srcMultiAppl used on empty list"
srcMultiAppl xs = concat $ intersperse " " $ map parens xs

srcSummate :: TinyBangCode
srcSummate =
  "this -> x ->" ++
  tbCase "x == 0" ["`True _ -> 0", "`False _ -> x + this (x - 1)"]

srcGreaterOrLessUtil :: TinyBangCode
srcGreaterOrLessUtil =
 "this -> x -> y -> z ->" ++
 tbCase "(x - y) == z"
   [ "`True _ -> `GreaterThan ()"
   , "`False _ -> " ++ tbCase "(y - x) == z"
                         [ "`True _ -> `LessThan ()"
                         , "`False _ -> this x y (z + 1)"]]

srcGreaterOrLess :: TinyBangCode
srcGreaterOrLess =
 "x -> y ->" ++
 tbCase "x == y" [ "`True _ -> `EqualTo ()"
                 , "`False _ -> " ++
                   srcMultiAppl [srcY, srcGreaterOrLessUtil, "x", "y", "1"]]

lblEq, lblLt, lblGt :: Result
lblEq = (A.VLabel (labelName "EqualTo") 0, makeState [(0,A.VPrimUnit)])
lblLt = (A.VLabel (labelName "LessThan") 0, makeState [(0,A.VPrimUnit)])
lblGt = (A.VLabel (labelName "GreaterThan") 0, makeState [(0,A.VPrimUnit)])
