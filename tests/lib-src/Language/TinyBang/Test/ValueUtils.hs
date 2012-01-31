module Language.TinyBang.Test.ValueUtils
( pi
, identFuncX
)
where

import Prelude ()

--import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.NameUtils
import Language.TinyBang.Test.ExpressionUtils
  ( varX
  )
import qualified Language.TinyBang.Ast as A (Value (VPrimInt, VFunc))

pi = A.VPrimInt

identFuncX = A.VFunc idX varX