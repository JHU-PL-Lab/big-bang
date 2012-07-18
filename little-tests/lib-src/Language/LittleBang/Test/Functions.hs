module Language.LittleBang.Test.Functions
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.NameUtils
  ( idX, idSelf
  )
import Language.LittleBang.Test.ExpressionUtils
  ( varX, varSelf, simplePat
  )
import Language.LittleBang.Test.ValueUtils
  ( identFuncX
  )
import Language.LittleBang.Test.SourceUtils
  ( srcY
  , srcMultiAppl
  )

import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg
import Data.ExtensibleVariant

selfIdentFuncX = TA.VScape (simplePat idSelf) $ TA.scape (simplePat idX) varX

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Test functions" $ TestList
  [ xvEval "x -> x"
          selfIdentFuncX
  , xvEval "(x -> x) (x -> x)"
          selfIdentFuncX
  , xvEval "(y -> y) (x -> x)"
          selfIdentFuncX
  , xvEval "def x = x -> x in x x"
          selfIdentFuncX
  , xType srcY
  , xvEval "x -> x x"
          (TA.VScape (simplePat idSelf) $ TA.scape (simplePat idX) $
            TA.appl (TA.appl varX $ TA.emptyOnion) varX)
  , xType "(x -> x x) (x -> x x)"
  , xType "def omega = x -> x x in omega omega"

  -- Ensure that functions are correctly polymorphic when stored in cells
  , xType "def f = (x -> x) in (f 1) & (f ())"

  -- Ensure that constraints propogate into functions properly
  , xCont "(f -> f ()) (x -> 1 + x)"
  , xCont "(x -> x + 1) 'a'"

  -- Test that application requires that the first argument be a function
  , xCont "1 'x'"

  -- Ensure that constraints from functions propagate correctly from cells
  , xCont "def f = (x -> (int -> x) x) in (f 1) & (f ())"

  -- Test typechecking of some pathological functions
  , xType $ srcMultiAppl
      [srcY, "this -> x -> this (`A x & `B x)"]
  , xType $ srcMultiAppl
      [srcY, "this -> x -> this (`A x & `B x)", "0"]
  , xType $ srcMultiAppl
      [srcY, "this -> x -> this (`A x & `B x)", "()"]
  , xType $ srcMultiAppl
      [srcY, "this -> x -> this (`A x & `B x)", "`A () & `B ()"]
  , xType $ srcMultiAppl
      [srcY, "this -> x -> this (`A x & `B x)", srcY]

  -- Check that variable closed-ness works in functions
  , xNotC "(x -> n + 2)"
  , xPars "x -> ((`False a -> 0) & (`True a -> 1)) x"
          (LA.scape (simplePat idX) $
            LA.appl (
                LA.onion
                  (LA.scape (TA.Pattern (ident "_") $
                      TA.PatLabel (labelName "False") (ident "a") $
                        TA.PatOnion []
                    ) $ TA.primInt 0 )
                  (LA.scape (TA.Pattern (ident "_") $
                      TA.PatLabel (labelName "True") (ident "a") $
                        TA.PatOnion []
                    ) $ TA.primInt 1 )
              ) $ TA.var idX)
  ]
