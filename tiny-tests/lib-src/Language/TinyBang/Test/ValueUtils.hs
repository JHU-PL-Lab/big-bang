module Language.TinyBang.Test.ValueUtils
( pi
, identFuncX
, true
, false
)
where

import Prelude (Integer, ($), (.))

import qualified Data.IntMap as IntMap
import Language.TinyBang.Test.ExpressionUtils
  ( varX
  )
import Language.TinyBang.Test.NameUtils
import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Interpreter.Ast as IA
import Utils.Language.Ast

pi :: Integer -> A.Value IA.Expr
pi = A.VPrimInt

identFuncX :: A.Value IA.Expr
identFuncX = A.VFunc idX varX

-- The following should only be used when there is no other expected state.
true :: (A.Value IA.Expr, IntMap.IntMap (A.Value IA.Expr))
true = (A.VLabel lblTrue 0, makeState [(0, A.VPrimUnit)])
false :: (A.Value IA.Expr, IntMap.IntMap (A.Value IA.Expr))
false = (A.VLabel lblFalse 0, makeState [(0, A.VPrimUnit)])
