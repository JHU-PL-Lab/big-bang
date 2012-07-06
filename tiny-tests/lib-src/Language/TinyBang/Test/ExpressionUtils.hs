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
, multiOnion
, patAny
, patInt
, patChar
, patUnit
, patFun
, simpleScape
, simpleDef
, simpleAssign
, simpleLabel
)
where

import Prelude hiding (pi)

--import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.NameUtils
import qualified Language.TinyBang.Ast as A
  (Expr, ExprPart(..), Pattern(..), PrimaryPattern(..))
import Data.ExtensibleVariant
import Language.TinyBang.Test.UtilFunctions (ident, labelName)
import qualified Language.TinyBang.Types.Types as T

varX = inj $ A.Var idX
varY = inj $ A.Var idY
varZ = inj $ A.Var idZ

patAny = A.PatOnion []

identFuncX = inj $ A.Scape (A.Pattern idX patAny) varX

pi = inj . A.PrimInt

false = inj $ A.Label lblFalse Nothing $ inj $ A.PrimUnit
true = inj $ A.Label lblTrue Nothing $ inj $ A.PrimUnit

multiAppl :: [A.Expr] -> A.Expr
multiAppl = foldl1 (\x y -> inj $ A.Appl x y)

multiOnion :: [A.Expr] -> A.Expr
multiOnion = foldl1 (\x y -> inj $ A.Onion x y)

simpleScape :: String -> A.Expr -> A.Expr
simpleScape x =
  inj . A.Scape (A.Pattern (ident x) (A.PatOnion []))

simpleDef :: String -> A.Expr -> A.Expr -> A.Expr
simpleDef x e =
  inj . A.Def Nothing (ident x) e

simpleAssign :: String -> A.Expr -> A.Expr -> A.Expr
simpleAssign x e =
  inj . A.Assign (ident x) e

simpleLabel :: String -> A.Expr -> A.Expr
simpleLabel x =
  inj . A.Label (labelName x) Nothing

patInt :: A.PrimaryPattern
patInt = A.PatPrim T.PrimInt

patChar :: A.PrimaryPattern
patChar = A.PatPrim T.PrimChar

patUnit :: A.PrimaryPattern
patUnit = A.PatPrim T.PrimUnit

patFun :: A.PrimaryPattern
patFun = A.PatFun