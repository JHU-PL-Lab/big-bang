module Language.TinyBang.Test.NameUtils
( idX
, idY
, idZ
, id_
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

import Language.TinyBang.Test.UtilFunctions
  ( ident
  , labelName
  )

idX = ident "x"
idY = ident "y"
idZ = ident "z"
id_ = ident "_"

lblA = labelName "A"
lblB = labelName "B"
lblC = labelName "C"
lblS = labelName "S"
lblZ = labelName "Z"

lblRef = labelName "Ref"
lblTrue = labelName "True"
lblFalse = labelName "False"