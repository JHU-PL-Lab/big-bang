module Language.TinyBang.Test.EagerOps
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Ast as A

tests = TestLabel "Eager operations tests" $ TestList
  [
-- TODO: uncomment when eager ops work
  -- , xType (srcMultiAppl [srcY, srcSummate, "5"])
  -- , xType (srcMultiAppl [srcGreaterOrLess, "4", "4"])
  -- , xCont (srcMultiAppl [srcGreaterOrLess, "`A 4", "4"])
  -- , xCont (srcMultiAppl [srcGreaterOrLess, "'a'", "4"])
  -- , xType (srcMultiAppl [srcGreaterOrLess, "'a'"])
-- Test evaluation of some recursive arithmetic evaluations
-- TODO: uncomment when we define equal
--  , xEval (srcMultiAppl [srcY, srcSummate, "5"]) $
--          A.VPrimInt 15
-- , xCont (srcMultiAppl [srcY, srcGreaterOrLess, "4", "4"])
-- , xEval (srcMultiAppl [srcGreaterOrLess, "4", "4"]) $
--         lblEq
-- , xEval (srcMultiAppl [srcGreaterOrLess, "0", "4"]) $
--         lblLt
-- , xEval (srcMultiAppl [srcGreaterOrLess, "4", "0"]) $
--         lblGt

-- -- Test simple equalities
-- TODO: uncomment when we have equal
--   , xType "equal 1 1"
--   , xEval "equal 1 1"
--           true
--   , xEval "equal 0 1"
--           false
--   , xEval "equal 0 (minus (minus (plus 1 1) 1) 1)"
--           true
--   , xType "equal 'a' 'a'"
--   , xEval "equal 'a' 'a'"
--           true
--   , xEval "equal 'a' 'A'"
--           false
--   , xEval "equal `True () `True ()"
--           true
--   , xEval "equal `A 1 `A 1"
--           true
--   , xType "equal () ()"
--   , xEval "equal () ()"
--           true
-- -- TODO: make these tests pass by implementing the equality constraint
--   , xCont "equal `A 1 `B 1"
--   , xCont "equal `True () `False ()"
--   , xCont "equal 1 'a'"
--   , xCont "equal 1 ()"
--   , xCont "equal (fun x -> x) (fun y -> y)"
--   , xType "(fun f -> equal f f) (fun x -> x)"
--   , xEval "(fun f -> equal f f) (fun x -> x)" $
--           true
-- -- Test equality evaluations on onions
-- -- TODO: Make all of these pass
--   , xEval "equal (1 & 'a') ('a' & 1)"
--           true
--   , xEval "equal ('a' & 1) (1 & 'a')"
--           true
--   , xEval "equal (1 & 'a') (1 & 'z')"
--           false
--   , xEval "equal (1 & 'a') (0 & 'a')"
--           false
--   , xEval "equal (1 & 2) (2 & 1)"
--           false
  ]
