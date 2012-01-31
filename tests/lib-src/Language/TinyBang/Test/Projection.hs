module Language.TinyBang.Test.Projection
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A

-- TODO: write a quickcheck that "case `lbl e of { `lbl x -> x }" == e

zero = A.VPrimInt 0
one = A.VPrimInt 1

tests = TestLabel "Test of projection, both implicit and explicit" $ TestList
  [ xEval "case `A 5 & `A 'a' of {`A x -> x}"
          (A.VPrimChar 'a')
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

  -- Test that implicit projection of functions fails
  , xCont "(1 & (fun x -> x)) 1"

  -- Test that implicit projection of lazy ops fails
  , xCont "[+] (2 & 'b') 2"
  , xCont "[+] (`True () & 'z') 2"
  , xCont "[+] (2 & 'x') ('y' & 2)"
  , xCont "[+] (2 & ('a' & ())) ((2 & 'b') & ())"
  , xCont "[+] (1 & ('a' & ())) ('a' & (1 & ()))"
  , xCont "[+] (1 & 'a') ('a' & 1 & ())"
  ]
