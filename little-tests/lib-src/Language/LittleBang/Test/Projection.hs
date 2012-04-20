module Language.LittleBang.Test.Projection
( tests
)
where

import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg

-- TODO: write a quickcheck that "case `lbl e of { `lbl x -> x }" == e

zero = TA.VPrimInt 0
one = TA.VPrimInt 1
two = TA.VPrimInt 2
four = TA.VPrimInt 4

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Test of projection, both implicit and explicit" $ TestList
  [ xEval "case `A 5 & `A 'a' of {`A x -> x}"
          (TA.VPrimChar 'a')
  , xEval "case `A 'a' & `A 1 of {`A x -> x}"
          one
  , xEval "case 'a' of {char -> 0}"
          zero
  , xEval "case 1234567890 of {int -> 0}"
          zero
  , xEval "case (fun x -> x) of {fun -> 0}"
          zero
  , xEval "case (\\x -> x) of {fun -> 0}"
          zero
  , xEval "case () of {unit -> 0}"
          zero
  , xEval "case `Test () of {`Test a -> 0}"
          zero
  , xEval "case `B 2 of {`A x -> 0; `B y -> 1}"
          one
  , xEval "case `A 0 of {`A n -> n}"
          zero
  , xEval "case `A 1 of {`A a -> 1; `A n -> 0}"
          one
  , xType "case `A 5 of { `A x -> x }"

  -- Test that implicit projection of functions succeeds
  , xEval "(1 & (fun x -> x)) 1"
          one

  -- Test that implicit projection of lazy ops succeeds
  , xEval "(2 & 'b') + 2" $
          four
  , xCont "(`True () & 'z') + 2"
  , xEval "(2 & 'x') + ('y' & 2)" $
          four
  , xEval "(2 & ('a' & ())) + ((2 & 'b') & ())" $
          four
  , xEval "(1 & ('a' & ())) + ('a' & (1 & ()))" $
          two
  , xEval "(1 & 'a') + ('a' & 1 & ())" $
          two
  , xEval "1 + (1 & 2 & 3 & ())" $
          four
  ]
