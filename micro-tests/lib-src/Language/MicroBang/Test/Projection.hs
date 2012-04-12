module Language.MicroBang.Test.Projection
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Ast as A

-- TODO: write a quickcheck that "case `lbl e of { `lbl x -> x }" == e

zero = A.VPrimInt 0
one = A.VPrimInt 1
two = A.VPrimInt 2
four = A.VPrimInt 4

tests :: (?debug :: Bool) => Test
tests = TestLabel "Test of projection, both implicit and explicit" $ TestList
  [
  xEval "case `A 5 & `A () of {`A x -> x}"
          (A.VPrimUnit)
  , xEval "case `A () & `A 1 of {`A x -> x}"
          one
  --, xEval "case 'a' of {char -> 0}"
  --        zero
  , xEval "case 1234567890 of {int -> 0}"
          zero
  , xEval "case (fun x -> x) of {fun -> 0}"
          zero
  , xEval "case (fun x -> x) of {fun -> 0}"
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
  --, xType "case `A 5 of { `A x -> x }"

  -- Test that implicit projection of functions succeeds
  , xEval "(1 & (fun x -> x)) 1"
          one

  -- Test that implicit projection of lazy ops succeeds
  , xEval "[+] (2 & ()) 2" $
          four
  , xCont "[+] (`True () & ()) 2"
  , xEval "[+] (2 & `X ()) (`Y 5 & 2)" $
          four
  , xEval "[+] (2 & (`A 6 & ())) ((2 & `B 7) & ())" $
          four
  , xEval "[+] (1 & (`A 19 & ())) (`A 67 & (1 & ()))" $
          two
  , xEval "[+] (1 & `A 42) (`B 57 & 1 & ())" $
          two
  , xEval "[+] 1 (1 & 2 & 3 & ())" $
          four
  ]