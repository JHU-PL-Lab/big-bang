module Language.LittleBang.Test.ValueUtils
( pi
, identFuncX
, true
, false
)
where

import Prelude hiding (pi)

import Language.LittleBang.Test.NameUtils
import Language.LittleBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Types.UtilTypes as TUT

pi = TA.VPrimInt

identFuncX = TA.VFunc tidX $ TA.Var $ TUT.ident "x"

-- The following should only be used when there is no other expected state.
true = (TA.VLabel tlblTrue 0, makeState [(0, TA.VPrimUnit)])
false = (TA.VLabel tlblFalse 0, makeState [(0, TA.VPrimUnit)])
