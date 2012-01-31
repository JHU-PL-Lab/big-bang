module Language.TinyBang.Test.ExpressionUtils
( varX
, varY
, varZ
, identFuncX
, pi
, false
, true
, multiAppl
)
where

import Prelude hiding (pi)

--import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.NameUtils
import qualified Language.TinyBang.Ast as A
  (Expr (Var, Func, PrimInt, PrimUnit, Label, Appl))

varX = A.Var idX
varY = A.Var idY
varZ = A.Var idZ

identFuncX = A.Func idX varX

pi = A.PrimInt

false = A.Label lblFalse A.PrimUnit
true = A.Label lblTrue A.PrimUnit

multiAppl = foldl1 A.Appl