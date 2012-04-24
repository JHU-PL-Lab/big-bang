module Language.MicroBang.Test.EagerOps
( tests
)
where

import Language.MicroBang.Test.SourceUtils
import Language.MicroBang.Test.UtilFunctions
import Language.MicroBang.Test.ValueUtils (true,false)
import qualified Language.MicroBang.Ast as A



xEvalComp :: (?conf :: Bool) => Ordering -> String -> String -> [Test]
xEvalComp ord srcA srcB =
  [ xEval ("[=] (" ++ srcA ++ ") (" ++ srcB ++ ")") $
          if ord == EQ then true else false
  , xEval ("[=] (" ++ srcB ++ ") (" ++ srcA ++ ")") $
          if ord == EQ then true else false
  ]

tests :: (?conf :: Bool) => Test
tests = TestLabel "Eager operations tests" $ TestList $ concat
  [ [ xCont (srcMultiAppl [srcGreaterOrLess, "`A 4", "4"])
    , xCont (srcMultiAppl [srcGreaterOrLess, "()", "4"])
    , xCont (srcMultiAppl [srcGreaterOrLess, "()", "`A 4"])
    , xCont (srcMultiAppl [srcGreaterOrLess, "`A 4", "`A 4"])
    , xCont (srcMultiAppl [srcGreaterOrLess, "()", "()"])
    , xCont (srcMultiAppl [srcGreaterOrLess, "`B ()", "`A ()"])
    , xCont (srcMultiAppl [srcGreaterOrLess, "()", "fun x -> x"])
    , xCont (srcMultiAppl [srcGreaterOrLess, "fun y -> y", "fun x -> x"])

  -- Test evaluation of some recursive arithmetic evaluations
    , xEval (srcMultiAppl [srcY, srcSummate, "5"]) $
             A.VPrimInt 15
    , xEval (srcMultiAppl [srcGreaterOrLess, "4", "4"]) $
            lblEq
    , xEval (srcMultiAppl [srcGreaterOrLess, "0", "4"]) $
            lblLt
    , xEval (srcMultiAppl [srcGreaterOrLess, "4", "0"]) $
            lblGt
    ]
-- Test simple comparisons
  , xEvalComp EQ "1" "1"
  , xEvalComp LT "0" "1"
  , xEvalComp EQ
      "0"
      "[+] ([+] ([+] 1 1) -1) -1"
-- Test onion orderings
  , xEvalComp EQ "1 & ()" "() & 1"
  , xEvalComp LT "1 & ()" "1 & 2"
  , xEvalComp GT "1 & `Z ()" "2 & `A ()"
  , xEvalComp GT "1 & 2" "2 & 1"
  , xEvalComp GT "1 & ()" "0 & `A ()"
  , xEvalComp LT "() & 1" "2"
  , xEvalComp EQ "0" "`A () & 0"
  ]