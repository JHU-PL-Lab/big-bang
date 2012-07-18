{-# LANGUAGE  NoMonomorphismRestriction #-}

module Language.LittleBang.Test.ValueUtils
( pi
, identFuncX
, true
, false
)
where

import Prelude (Integer)

import qualified Data.IntMap as IntMap
import Language.LittleBang.Test.ExpressionUtils
  ( varX
  , simplePat
  )
import Language.LittleBang.Test.NameUtils
import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Interpreter.Ast as IA

pi :: Integer -> TA.Value IA.Expr
pi = TA.VPrimInt

identFuncX = TA.VScape (simplePat idX) varX

-- The following should only be used when there is no other expected state.
true = (TA.VLabel lblTrue 0, makeState [(0, TA.VPrimUnit)])
false = (TA.VLabel lblFalse 0, makeState [(0, TA.VPrimUnit)])
