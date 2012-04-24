module Language.LittleBang.Test.ExpressionUtils
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

--import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.NameUtils
import qualified Language.LittleBang.Ast as LA

varX = LA.Var lidX
varY = LA.Var lidY
varZ = LA.Var lidZ

identFuncX = LA.Func lidX varX

pi = LA.PrimInt

false = LA.Label llblFalse Nothing LA.PrimUnit
true = LA.Label llblTrue Nothing LA.PrimUnit

multiAppl = foldl1 LA.Appl
