module Language.MicroBang.Test.ValueUtils
( pi
, identFuncX
, true
, false
)
where

import Prelude ()

import Language.MicroBang.Test.ExpressionUtils
  ( varX
  )
import Language.MicroBang.Test.NameUtils
import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Ast as A

pi = A.VPrimInt

identFuncX = A.VFunc idX varX

-- The following should only be used when there is no other expected state.
true = (A.VLabel lblTrue A.VPrimUnit)
false = (A.VLabel lblFalse A.VPrimUnit)