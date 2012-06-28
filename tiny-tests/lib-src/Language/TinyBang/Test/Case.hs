module Language.TinyBang.Test.Case
( tests
)
where

import Language.TinyBang.Test.SourceUtils
import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Test.ExpressionUtils as E
  ( false
  )
import Language.TinyBang.Test.ExpressionUtils
  ( varX
  )
import qualified Language.TinyBang.Test.ValueUtils as V
  ( pi
  )
import Language.TinyBang.Test.NameUtils
  ( idX
  , lblTrue
  )

import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Types.Types as T

import Utils.Language.Ast

--caseEval :: String -> [String] ->
caseEval e bs v = xEval (tbCase e bs) v

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "General case tests" $ TestList
  [ caseEval "1" ["int -> 0"] (V.pi 0)
  , caseEval "1" ["x:int -> 0"] (V.pi 0)

  -- Test that incomplete case statements result in contradiction
  , xCont "(char -> 0) 1"
  , xCont "(x:char -> 0) 1"
  , xCont "(`B x -> x) (`A 4)"
  , xCont "(y:`B x -> x) (`A 4)"

  -- ensure that closedness works in case expressions
  , xNotC $ tbCase "x" ["int -> 0", "char -> 'a'"]
  , xNotC $ tbCase "4" ["int -> x"]
  , xNotC $ tbCase "4" ["int -> ()", "unit -> x"]

  -- verify the behavior of the fun pattern
  , caseEval "1" ["fun -> 0", "z -> z"] (V.pi 1)
  , caseEval "x -> x" ["int -> 0", "fun -> 1"] (V.pi 1)
  , xCont "(fun -> 0) (`A 0)"

  -- Check to make sure that path sensitivity does not exist, as we don't
  -- expect it to in the recursive case.
  , xCont $ srcMultiAppl
      [ srcY
      , tbScape ["this", "v"] $
          tbCase "v" [ "unit -> 0"
                     , "`A _ -> (`B y -> 1 + (this y)) v"]
      , "`A 0 & `B (`A 0 & `B ())"]

   , xCont $ tbDef "z"
       (srcMultiAppl [ srcY
                     , tbScape ["this", "x"] $
                         tbCase "x" ["`A y -> `X (this y)"
                                    ,"`B y -> `Y (this y)"
                                    ,"int -> x"]
                     , "`A `B `A 0"])
       "(`A q -> q) z"

  -- This unit test fails and we're not sure why
  , xCont $ tbDef "z"
       (srcMultiAppl [ srcY
                     , tbScape ["this", "x"] $
                         tbCase "x" ["`A y -> `X (this y)"
                                    ,"`B y -> `Y (this y)"
                                    ,"int -> x"]
                     , "`A `B `A 0"])
       "(`X q -> q) z"

  , xCont $ tbDef "z"
       (srcMultiAppl [ srcY
                     , tbScape ["this", "x"] $
                         tbCase "x" ["`A y -> `X (this y)"
                                    ,"`B y -> `Y (this y)"
                                    ,"int -> x"]
                     , "`A `B `A 0"])
       "(`X `Y `X q -> q) z"

  -- Check broad patterns
  , xEval "((`A x & `B y) -> x + y) (`A 1 & `B 2)"
        $ V.pi 3
  , xEval "((`B x & `A y) -> x + y) (`A 2 & `B 3 & `C 9)"
        $ V.pi 5
  , xEval "(`A i -> i) (`A 3 & `B ())"
        $ V.pi 3
  , caseEval "`A 1 & `B 2"
      [ "`C x -> x"
      , "(`A x & `Z z) -> 6 + z"
      , "(`A x & `B y) -> x + y"
      , "`A x -> 9"]
      (V.pi 3)
  , xCont $ tbCase "`A 1 & `B 2" ["(`A x & `B y & `C z) -> 0"]
  , xCont $ tbCase "`A 1 & `B 2" ["(`A x & unit) -> 0"]

  -- Check deep patterns
  , caseEval
      "`A (`B 1 & `C 2) & `D (`E 4)"
      ["`A (`B x & `C y) -> x + y"]
        $ V.pi 3
  , caseEval
      "`A (`B 1 & `C 2) & `D (`E 4)"
      ["((`A `B x) & (`D `E y)) -> x + y"]
        $ V.pi 5
  , caseEval
      "`A (`B 1 & `C 2) & `D (`E 4)"
      ["(`A (`B x & `C y) & (`D _)) -> x + y"]
        $ V.pi 3
  , xCont $ tbCase
      "`A (`B 1 & `C 2) & `D (`E 4)"
      ["`A (`B _ & `Z x) -> x"]
  , xCont $ tbCase
      "`A (`B (`C 0))"
      ["`A (`C z) -> z"]

  -- Ensure that deep patterns bind the entire content without filtering.
  , caseEval
      "`A 1 & `B 2"
      ["x:`A _ -> " ++
         tbCase "x" ["(`A y & `B z) -> y + z"]]
        $ V.pi 3

  -- Ensure that outer pattern binders replicate structure rather than the cell
  -- itself.
  , xEval (tbDef "x" "1" $ tbCase "x" ["y:int -> x = 2 in y"])
        $ V.pi 1

  -- Verify pattern checking.
  , xDLbl $ tbCase "`A 0" ["(`A x & `A y) -> 0"]
  , xDBnd $ tbCase "`A 0 & `B 0" ["(`A x & `B x) -> 0"]
  , caseEval "`A `A `A 4" ["`A `A `A x -> x"]
        $ V.pi 4

  -- Confirm that pattern binders are properly substituted.
  , caseEval "0" ["x -> (x -> x + 1) 1"]
        $ V.pi 2

  -- TODO: we require more unit tests!  (verify inner binder mutation, etc.)
  ]
