module Language.TinyBang.Test.Projection
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.SourceUtils
  ( tbCase
  , tbDef)
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Interpreter.Ast as IA
import Data.ExtensibleVariant

-- TODO: write a quickcheck that "case `lbl e of { `lbl x -> x }" == e

zero :: A.Value IA.Expr
zero = A.VPrimInt 0
one :: A.Value IA.Expr
one = A.VPrimInt 1
two :: A.Value IA.Expr
two = A.VPrimInt 2
four :: A.Value IA.Expr
four = A.VPrimInt 4

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Test of projection, both implicit and explicit" $ TestList
  [ xvEval "(`A x -> x) (`A 5 & `A 'a')"
        (A.VPrimChar 'a')
  , xvEval "(`A x -> x) (`A 'a' & `A 1)"
        one
  , xvEval "(char -> 0) 'a'"
        zero
  , xvEval "(int -> 0) 1234567890"
        zero
  , xvEval "(fun -> 0) (x -> x)"
        zero
  , xvEval "(unit -> 0) ()"
        zero
  , xvEval "(`Test a -> 0) `Test ()"
        zero
  , xvEval (tbCase "`B 2" ["`A x -> 0", "`B y -> 1"])
        one
  , xvEval "(`A n -> n) `A 0"
        zero
  , xvEval (tbCase "`A 1" ["`A a -> 1", "`A n -> 0"])
        one
  , xType "(`A x -> x) `A 5"

  -- Test that implicit projection of functions succeeds
  , xvEval "(2 & (x -> x)) 1"
        one

  -- Test that implicit projection of lazy ops succeeds
  , xvEval "(2 & 'b') + 2" $
        four
  , xCont "(`True () & 'z') + 2"
  , xvEval "(2 & 'x') + ('y' & 2)" $
        four
  , xvEval "(2 & ('a' & ())) + ((2 & 'b') & ())" $
        four
  , xvEval "(1 & ('a' & ())) + ('a' & (1 & ()))" $
        two
  , xvEval "(1 & 'a') + ('a' & 1 & ())" $
        two
  , xvEval "1 + (1 & 2 & 3 & ())" $
        four

  -- Test that onion projection is polymorphic.
  -- (Verifies that the onion projection type is properly substituted)
  , xvEval (tbDef
              "f"
              "x -> x &. int"
              "f () & (f 1 + f 1)")
        two

  -- Test that onion subtraction is polymorphic.
  -- (Verifies that the onion subtraction type is properly substituted)
  , xvEval (tbDef
              "f"
              "x -> x &- unit"
              "f () & (f 1 + 1)")
        two
  ]