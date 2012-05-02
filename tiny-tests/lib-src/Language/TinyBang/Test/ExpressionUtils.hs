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
import qualified Language.TinyBang.Ast as A (Expr, ExprPart(..))
import Utils.Language.Ast

varX :: A.Expr
varX = astwrap $ A.Var idX
varY :: A.Expr
varY = astwrap $ A.Var idY
varZ :: A.Expr
varZ = astwrap $ A.Var idZ

identFuncX :: A.Expr
identFuncX = astwrap $ A.Func idX varX

pi :: Integer -> A.Expr
pi = astwrap . A.PrimInt

false :: A.Expr
false = astwrap $ A.Label lblFalse Nothing $ astwrap $ A.PrimUnit
true :: A.Expr
true = astwrap $ A.Label lblTrue Nothing $ astwrap $ A.PrimUnit

multiAppl :: [A.Expr] -> A.Expr
multiAppl = foldl1 (\x y -> astwrap $ A.Appl x y)
