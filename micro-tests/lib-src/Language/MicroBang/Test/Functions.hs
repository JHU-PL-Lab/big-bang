module Language.MicroBang.Test.Functions
( tests
)
where

import Language.MicroBang.Test.UtilFunctions
import Language.MicroBang.Test.NameUtils
  ( idX
  )
import Language.MicroBang.Test.ExpressionUtils
  ( varX
  )
import Language.MicroBang.Test.ValueUtils
  ( identFuncX
  )
import Language.MicroBang.Test.SourceUtils
  ( srcY
  , srcMultiAppl
  )

import qualified Language.MicroBang.Ast as A

tests :: (?conf :: Bool) => Test
tests = TestLabel "Test functions" $ TestList
  [ xEval "fun x -> x"
          identFuncX
  , xEval "(fun x -> x) (fun x -> x)"
          identFuncX
  , xEval "(fun y -> y) (fun x -> x)"
          identFuncX
  , xEval "fun x -> x x"
          (A.VFunc)

  -- Ensure that constraints propogate into functions properly
  , xCont "(fun f -> f ()) (fun x -> [+] 1 x)"
  , xCont "(fun x -> [+] x 1) ()"

  -- Test that application requires that the first argument be a function
  , xCont "1 ()"

  -- Check that variable closed-ness works in functions
  , xCont "(fun x -> [+] n 2)"
  ]