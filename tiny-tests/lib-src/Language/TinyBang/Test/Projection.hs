module Language.TinyBang.Test.Projection
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import Utils.Language.Ast

-- TODO: write a quickcheck that "case `lbl e of { `lbl x -> x }" == e

zero :: A.Value A.Expr
zero = A.VPrimInt 0
one :: A.Value A.Expr
one = A.VPrimInt 1
two :: A.Value A.Expr
two = A.VPrimInt 2
four :: A.Value A.Expr
four = A.VPrimInt 4

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Test of projection, both implicit and explicit" $ TestList
  [ xvEval "case `A 5 & `A 'a' of {`A x -> x}"
          (A.VPrimChar 'a')
  , xvEval "case `A 'a' & `A 1 of {`A x -> x}"
          one
  , xvEval "case 'a' of {char -> 0}"
          zero
  , xvEval "case 1234567890 of {int -> 0}"
          zero
  , xvEval "case (fun x -> x) of {fun -> 0}"
          zero
  , xvEval "case (\\x -> x) of {fun -> 0}"
          zero
  , xvEval "case () of {unit -> 0}"
          zero
  , xvEval "case `Test () of {`Test a -> 0}"
          zero
  , xvEval "case `B 2 of {`A x -> 0; `B y -> 1}"
          one
  , xvEval "case `A 0 of {`A n -> n}"
          zero
  , xvEval "case `A 1 of {`A a -> 1; `A n -> 0}"
          one
  , xType "case `A 5 of { `A x -> x }"

  -- Test that implicit projection of functions succeeds
  , xvEval "(1 & (fun x -> x)) 1"
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
  ]
