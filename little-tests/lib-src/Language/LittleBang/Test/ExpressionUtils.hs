{-# LANGUAGE  NoMonomorphismRestriction
            #-}

module Language.LittleBang.Test.ExpressionUtils
( varX
, varY
, varZ
, varSelf
, simplePat
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
import Data.ExtensibleVariant

varX = inj $ TA.Var idX
varY = inj $ TA.Var idY
varZ = inj $ TA.Var idZ
varSelf = inj $ TA.Var idSelf

simplePat ident = TA.Pattern ident $ TA.PatOnion []

identFuncX = TA.scape (simplePat idX) varX

pi = inj . TA.PrimInt

false = inj $ TA.Label lblFalse Nothing $ inj $ TA.PrimUnit
true = inj $ TA.Label lblTrue Nothing $ inj $ TA.PrimUnit

multiAppl = foldl1 (\x y -> inj $ LA.Appl x y)
