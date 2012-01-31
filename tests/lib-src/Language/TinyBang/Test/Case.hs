module Language.TinyBang.Test.Case
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Test.ExpressionUtils as E
  ( false
  , true
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
import qualified Language.TinyBang.Types.Types as T

tests = TestLabel "General case tests" $ TestList
  [ xEval "case 1 of {int -> 0}"
          (V.pi 0)

  -- Test that incomplete case statements result in contradiction
  , xCont "case 1 of {char -> 0}"
  , xCont "case `A 4 of { `B x -> x }"

  -- ensure that closedness works in case expressions
  , xNotC "case x of {int -> 0; char -> 'a'}"
  , xNotC "case 4 of {int -> x}"
  , xNotC "case 4 of {int -> () ; unit -> x}"
  , xPars "case x of {\
                      \    int -> 5;             \
                      \    char -> 'a';          \
                      \    unit -> ();           \
                      \    `True a -> `False (); \
                      \    fun -> (fun x -> x)}" $
          A.Case varX
            [ A.Branch Nothing (A.ChiPrim T.PrimInt) $ A.PrimInt 5
            , A.Branch Nothing (A.ChiPrim T.PrimChar) $ A.PrimChar 'a'
            , A.Branch Nothing (A.ChiPrim T.PrimUnit) A.PrimUnit
            , A.Branch Nothing (A.ChiLabel lblTrue (ident "a")) E.false
            , A.Branch Nothing A.ChiFun $ A.Func idX varX
            ]
  ]