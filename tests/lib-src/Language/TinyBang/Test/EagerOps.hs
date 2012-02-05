module Language.TinyBang.Test.EagerOps
( tests
)
where

import Language.TinyBang.Test.SourceUtils
import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.ValueUtils (true,false)
import qualified Language.TinyBang.Ast as A

tests :: (?debug :: Bool) => Test
tests = TestLabel "Eager operations tests" $ TestList
  [ xType (srcMultiAppl [srcY, srcSummate, "5"])
  , xType (srcMultiAppl [srcGreaterOrLess, "4", "4"])
  , xType (srcMultiAppl [srcGreaterOrLess, "`A 4", "4"])
  , xType (srcMultiAppl [srcGreaterOrLess, "'a'", "4"])
  , xType (srcMultiAppl [srcGreaterOrLess, "'a'"])

-- Test evaluation of some recursive arithmetic evaluations
  , xEval (srcMultiAppl [srcY, srcSummate, "5"]) $
           A.VPrimInt 15
  , xEval (srcMultiAppl [srcGreaterOrLess, "4", "4"]) $
          lblEq
  , xEval (srcMultiAppl [srcGreaterOrLess, "0", "4"]) $
          lblLt
  , xEval (srcMultiAppl [srcGreaterOrLess, "4", "0"]) $
          lblGt

-- Test simple [=]ities
  , xType "[=] 1 1"
  , xEval "[=] 1 1"
          true
  , xEval "[=] 0 1"
          false
  , xEval "[=] 0 ([-] ([-] ([+] 1 1) 1) 1)"
          true
  , xType "[=] 'a' 'a'"
  , xEval "[=] 'a' 'a'"
          true
  , xEval "[=] 'a' 'A'"
          false
  , xEval "[=] `True () `True ()"
          true
  , xEval "[=] `A 1 `A 1"
          true
  , xType "[=] () ()"
  , xEval "[=] () ()"
          true
-- TODO: make these tests pass by implementing the [=]ity constraint
  , xType "[=] `A 1 `B 1"
  , xType "[=] `True () `False ()"
  , xType "[=] 1 'a'"
  , xType "[=] 1 ()"
  , xType "[=] (fun x -> x) (fun y -> y)"
  , xType "(fun f -> [=] f f) (fun x -> x)"
  , xEval "(fun f -> [=] f f) (fun x -> x)" $
          true
-- Test [=]ity evaluations on onions
-- TODO: Make all of these pass
  , xEval "[=] (1 & 'a') ('a' & 1)"
          true
  , xEval "[=] ('a' & 1) (1 & 'a')"
          true
  , xEval "[=] (1 & 'a') (1 & 'z')"
          false
  , xEval "[=] (1 & 'a') (0 & 'a')"
          false
  , xEval "[=] (1 & 2) (2 & 1)"
          false
  ]
