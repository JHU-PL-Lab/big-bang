module Language.LittleBang.Test.NameUtils
( idX
, idY
, idZ
, idSelf
, lblA
, lblB
, lblC
, lblS
, lblZ
, lblRef
, lblTrue
, lblFalse
)
where

import Language.LittleBang.Test.UtilFunctions
  ( ident
  , labelName
  )

idX = ident "x"
idY = ident "y"
idZ = ident "z"
idSelf = ident "self"

lblA = labelName "A"
lblB = labelName "B"
lblC = labelName "C"
lblS = labelName "S"
lblZ = labelName "Z"

lblRef = labelName "Ref"
lblTrue = labelName "True"
lblFalse = labelName "False"
