module Language.TinyBang.Test.Functions
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.NameUtils
  ( idX
  )
import Language.TinyBang.Test.ExpressionUtils
  ( varX
  )
import Language.TinyBang.Test.ValueUtils
  ( identFuncX
  )
import Language.TinyBang.Test.SourceUtils
  ( srcY
  , srcMultiAppl
  )

import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Data.ExtensibleVariant

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Test functions" $ TestList
  [ xEval "x -> x"
          identFuncX
  , xEval "(x -> x) (x -> x)"
          identFuncX
  , xEval "(y -> y) (x -> x)"
          identFuncX
  , xEval "def x = x -> x in x x"
          identFuncX
  , xType srcY
  , xEval "x -> x x"
      (A.VScape (A.Pattern idX $ A.PatOnion []) $
      (inj $ A.Appl varX varX :: IA.Expr))
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
  , xCont "def f = (x:int -> x ) in (f 1) & (f ())"

  -- Ensure that scape application does not expand overridden scapes
  , xType "((_ -> 0 0) & (_ -> 0)) 0"

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

  -- Check that constraints generated by tPatType are actually added
  -- to constraint set.
  , xCont "(this -> x -> (`A y -> this this `B y) x)      \
          \(this -> x -> (`A y -> this this `B y) x) `A ()"
  , xCont "(z -> x -> (`A y -> y + 1) x) 0 `A ()"

  -- Check to ensure that scape application typechecks correctly in light of
  -- multiflows
  , xType $ "def x = 0 in x = () in " ++
            "(((int -> 'z') & (int -> 0) & (unit -> 0)) x) + 1"
  , xType $ "def x = 0 in x = () in " ++
            "(((`A int -> 'z') & (`A int -> 0) & (`A unit -> 0)) `A x) + 1"

  -- Check to ensure that scape application typecheck errors are generated
  -- correctly in light of multiflows
  , xType "def x = 5 in x = () in ((unit -> 0) & (int -> 1)) x"
  , xType "def x = 5 in x = () in ((`A unit -> 0) & (`A int -> 1)) (`A x)"
  ]
