module Language.MicroBang.Test.NameUtils
( idX
, idY
, idZ
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

import Language.MicroBang.Test.UtilFunctions
  ( ident
  , labelName
  )

idX = ident "x"
idY = ident "y"
idZ = ident "z"

lblA = labelName "A"
lblB = labelName "B"
lblC = labelName "C"
lblS = labelName "S"
lblZ = labelName "Z"

lblRef = labelName "Ref"
lblTrue = labelName "True"
lblFalse = labelName "False"