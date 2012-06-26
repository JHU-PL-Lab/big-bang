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
import Utils.Language.Ast
import Language.TinyBang.Test.UtilFunctions (ident, labelName)
import qualified Language.TinyBang.Types.Types as T

varX = astwrap $ A.Var idX
varY = astwrap $ A.Var idY
varZ = astwrap $ A.Var idZ

patAny = A.PatOnion []

identFuncX = astwrap $ A.Scape (A.Pattern idX patAny) varX

pi = astwrap . A.PrimInt

false = astwrap $ A.Label lblFalse Nothing $ astwrap $ A.PrimUnit
true = astwrap $ A.Label lblTrue Nothing $ astwrap $ A.PrimUnit

multiAppl :: [A.Expr] -> A.Expr
multiAppl = foldl1 (\x y -> astwrap $ A.Appl x y)

multiOnion :: [A.Expr] -> A.Expr
multiOnion = foldl1 (\x y -> astwrap $ A.Onion x y)

simpleScape :: String -> A.Expr -> A.Expr
simpleScape x =
  astwrap . A.Scape (A.Pattern (ident x) (A.PatOnion []))

simpleDef :: String -> A.Expr -> A.Expr -> A.Expr
simpleDef x e =
  astwrap . A.Def Nothing (ident x) e

simpleAssign :: String -> A.Expr -> A.Expr -> A.Expr
simpleAssign x e =
  astwrap . A.Assign (ident x) e

simpleLabel :: String -> A.Expr -> A.Expr
simpleLabel x =
  astwrap . A.Label (labelName x) Nothing

patInt :: A.PrimaryPattern
patInt = A.PatPrim T.PrimInt

patChar :: A.PrimaryPattern
patChar = A.PatPrim T.PrimChar

patUnit :: A.PrimaryPattern
patUnit = A.PatPrim T.PrimUnit

patFun :: A.PrimaryPattern
patFun = A.PatFun
