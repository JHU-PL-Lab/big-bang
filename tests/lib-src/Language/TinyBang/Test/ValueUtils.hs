module Language.TinyBang.Test.ValueUtils
( pi
, identFuncX
, true
, false
)
where

import Prelude ()

import Language.TinyBang.Test.ExpressionUtils
  ( varX
  )
import Language.TinyBang.Test.NameUtils
import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A

pi = A.VPrimInt

identFuncX = A.VFunc idX varX

-- The following should only be used when there is no other expected state.
true = (A.VLabel lblTrue 0, makeState [(0, A.VPrimUnit)])
false = (A.VLabel lblFalse 0, makeState [(0, A.VPrimUnit)])
