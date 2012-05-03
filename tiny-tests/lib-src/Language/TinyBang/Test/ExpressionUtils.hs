{-# LANGUAGE  NoMonomorphismRestriction
            #-}

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

varX = astwrap $ A.Var idX
varY = astwrap $ A.Var idY
varZ = astwrap $ A.Var idZ

identFuncX = astwrap $ A.Func idX varX

pi = astwrap . A.PrimInt

false = astwrap $ A.Label lblFalse Nothing $ astwrap $ A.PrimUnit
true = astwrap $ A.Label lblTrue Nothing $ astwrap $ A.PrimUnit

multiAppl :: [A.Expr] -> A.Expr
multiAppl = foldl1 (\x y -> astwrap $ A.Appl x y)
