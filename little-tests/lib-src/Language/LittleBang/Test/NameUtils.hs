module Language.LittleBang.Test.NameUtils
( lidX
, lidY
, lidZ
, llblA
, llblB
, llblC
, llblS
, llblZ
, llblRef
, llblTrue
, llblFalse
, tidX
, tidY
, tidZ
, tlblA
, tlblB
, tlblC
, tlblS
, tlblZ
, tlblRef
, tlblTrue
, tlblFalse
)
where

import Language.LittleBang.Types.UtilTypes as LUT
import Language.TinyBang.Types.UtilTypes as TUT

lidX = LUT.ident "x"
lidY = LUT.ident "y"
lidZ = LUT.ident "z"

llblA = LUT.labelName "A"
llblB = LUT.labelName "B"
llblC = LUT.labelName "C"
llblS = LUT.labelName "S"
llblZ = LUT.labelName "Z"

llblRef = LUT.labelName "Ref"
llblTrue = LUT.labelName "True"
llblFalse = LUT.labelName "False"

tidX = TUT.ident "x"
tidY = TUT.ident "y"
tidZ = TUT.ident "z"

tlblA = TUT.labelName "A"
tlblB = TUT.labelName "B"
tlblC = TUT.labelName "C"
tlblS = TUT.labelName "S"
tlblZ = TUT.labelName "Z"

tlblRef = TUT.labelName "Ref"
tlblTrue = TUT.labelName "True"
tlblFalse = TUT.labelName "False"
