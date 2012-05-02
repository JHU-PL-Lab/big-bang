{-# LANGUAGE  NoMonomorphismRestriction
            #-}

module Language.LittleBang.Test.ExpressionUtils
( varX
, varY
, varZ
, varSelf
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
import qualified Language.TinyBang.Ast as TA
import Utils.Language.Ast

varX = astwrap $ TA.Var idX
varY = astwrap $ TA.Var idY
varZ = astwrap $ TA.Var idZ
varSelf = astwrap $ TA.Var idSelf

identFuncX = astwrap $ TA.Func idX varX

pi = astwrap . TA.PrimInt

false = astwrap $ TA.Label lblFalse Nothing $ astwrap $ TA.PrimUnit
true = astwrap $ TA.Label lblTrue Nothing $ astwrap $ TA.PrimUnit

multiAppl = foldl1 (\x y -> astwrap $ LA.Appl x y)
