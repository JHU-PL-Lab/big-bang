module Language.LittleBang.Test.EagerOps
( tests
)
where

import Language.LittleBang.Test.SourceUtils
import Language.LittleBang.Test.UtilFunctions
import Language.LittleBang.Test.ValueUtils (true,false)
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg

xEvalComp :: (?conf :: Cfg.Config) => Ordering -> String -> String -> [Test]
xEvalComp ord srcA srcB =
  [ xsEval ("(" ++ srcA ++ ") == (" ++ srcB ++ ")") $
          if ord == EQ then true else false
  , xsEval ("(" ++ srcB ++ ") == (" ++ srcA ++ ")") $
          if ord == EQ then true else false
  , xsEval ("(" ++ srcA ++ ") <= (" ++ srcB ++ ")") $
          if ord == GT then false else true
  , xsEval ("(" ++ srcB ++ ") <= (" ++ srcA ++ ")") $
          if ord == LT then false else true
  , xsEval ("(" ++ srcA ++ ") >= (" ++ srcB ++ ")") $
          if ord == LT then false else true
  , xsEval ("(" ++ srcB ++ ") >= (" ++ srcA ++ ")") $
          if ord == GT then false else true
  ]

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Eager operations tests" $ TestList $ concat
  [ [ xType (srcMultiAppl [srcY, srcSummate, "5"])
    , xType (srcMultiAppl [srcGreaterOrLess, "4", "4"])
    , xCont (srcMultiAppl [srcGreaterOrLess, "`A 4", "4"])
    , xCont (srcMultiAppl [srcGreaterOrLess, "'a'", "4"])
    , xType (srcMultiAppl [srcGreaterOrLess, "'a'"])

  -- Test evaluation of some recursive arithmetic evaluations
    , xvEval (srcMultiAppl [srcY, srcSummate, "5"]) $
             TA.VPrimInt 15
    , xsEval (srcMultiAppl [srcGreaterOrLess, "4", "4"]) $
            lblEq
    , xsEval (srcMultiAppl [srcGreaterOrLess, "0", "4"]) $
            lblLt
    , xsEval (srcMultiAppl [srcGreaterOrLess, "4", "0"]) $
            lblGt
    ]
-- Test simple comparisons
  , xEvalComp EQ "1" "1"
  , xEvalComp LT "0" "1"
  , xEvalComp EQ
      "0"
      "((1 + 1) - 1) - 1"
  , xEvalComp EQ "'a'" "'a'"
  , xEvalComp GT "'a'" "'A'"
  , xEvalComp EQ "`True ()" "`True ()"
  , xEvalComp LT "`A 1" "`A 2"
  , xEvalComp LT "`A 2" "`B 1"
  , xEvalComp EQ "()" "()"
-- Test cross-type orderings
  , xEvalComp LT "()" "1"
  , xEvalComp LT "1" "'a'"
  , xEvalComp LT "()" "'a'"
  , xEvalComp LT "()" "`A 0"
  , xEvalComp LT "1" "`A 0"
  , xEvalComp LT "'a'" "`A 0"
  , xEvalComp LT "()" "fun x -> x"
  , xEvalComp LT "5" "fun x -> x"
  , xEvalComp LT "'a'" "fun x -> x"
  , xEvalComp LT "`A 0" "fun x -> x"
-- Test function equality by identity
  , [ xsEval "(fun f -> f == f) (fun x -> x)" true
    , xsEval "(fun f -> f <= f) (fun x -> x)" true
    , xsEval "(fun f -> f >= f) (fun x -> x)" true ]
-- Test deep label comparisons
  , xEvalComp LT "`A ()" "`A 1"
  , xEvalComp LT "`A 'a'" "`A `A ()"
  , xEvalComp EQ "`A `A `A ()" "`A `A `A ()"
  , xEvalComp LT "`A `A `A 0" "`A `A `A 1"
-- Test onion orderings
  , xEvalComp EQ "1 & 'a'" "'a' & 1"
  , xEvalComp LT "1 & 'a'" "1 & 'z'"
  , xEvalComp GT "1 & 'z'" "2 & 'a'"
  , xEvalComp GT "'a' & 'c'" "'b'"
  , xEvalComp GT "1 & 2" "2 & 1"
  , xEvalComp GT "1 & 'a'" "0 & 'a'"
  , xEvalComp LT "() & 1" "'a'"
  , xEvalComp EQ "`A 0" "`A () & `A 0"
  , xEvalComp EQ
      "`A 1 & `B 2"
      "`B 1 & `A `B 0 & `A 1 & `B () & `B 2"
  ]