module Language.LittleBang.Test.Case
( tests
)
where

import Language.LittleBang.Test.SourceUtils
import Language.LittleBang.Test.UtilFunctions
import qualified Language.LittleBang.Test.ExpressionUtils as E
  ( false
  )
import Language.LittleBang.Test.ExpressionUtils
  ( varX
  )
import qualified Language.LittleBang.Test.ValueUtils as V
  ( pi
  )
import Language.LittleBang.Test.NameUtils
  ( idX
  , lblTrue
  )

import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cfg
import qualified Language.TinyBang.Types.Types as T

import Data.ExtensibleVariant

tests :: (?conf :: Cfg.Config) => Test
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
          (inj $ LA.Case varX
            [ TA.Branch (TA.ChiTopBind $ TA.ChiUnbound
                            (TA.ChiPrim T.PrimInt)) $ inj $ TA.PrimInt 5
            , TA.Branch (TA.ChiTopBind $ TA.ChiUnbound
                            (TA.ChiPrim T.PrimChar)) $ inj $ TA.PrimChar 'a'
            , TA.Branch (TA.ChiTopBind $ TA.ChiUnbound
                            (TA.ChiPrim T.PrimUnit)) $ inj $ TA.PrimUnit
            , TA.Branch (TA.ChiTopBind $ TA.ChiUnbound
                            (TA.ChiLabelShallow lblTrue $ ident "a")) E.false
            , TA.Branch (TA.ChiTopBind $ TA.ChiUnbound
                            TA.ChiFun) $ inj $ LA.Func idX varX
            ]
            :: LA.Expr)

  -- verify the behavior of the fun pattern
  , xEval "case 1 of { fun -> 0; z -> z }" (V.pi 1)
  , xEval "case (fun x -> x) of { int -> 0; fun -> 1 }" (V.pi 1)
  , xCont "case `A 0 of { fun -> 0 }"

  -- Check to make sure that path sensitivity does not exist, as we don't
  -- expect it to.
  , xCont (srcY ++ "(fun this -> fun v ->                       \
                   \    case v of {                             \
                   \      unit -> 0;                            \
                   \      `A _ ->                               \
                   \        case v of {                         \
                   \          `B y -> 1 + (this y)              \
                   \        }                                   \
                   \    }                                       \
                   \) (`A 0 & `B (`A 0 & `B ()))                ")
  -- Check broad patterns
  , xEval "case `A 1 & `B 2 of {                                \
          \  `A x & `B y -> x + y                               \
          \}                                                    "
        $ V.pi 3
  , xEval "case `A 2 & `B 3 & `C 9 of {                         \
          \  `B x & `A y -> x + y                               \
          \}                                                    "
        $ V.pi 5
  , xEval "case `A 3 & `B () of {                               \
          \  `A i -> i                                          \
          \}                                                    "
        $ V.pi 3
  , xEval "case `A 1 & `B 2 of {                                \
          \  `C x -> x ;                                        \
          \  `A x & `Z z -> 6 + z ;                             \
          \  `A x & `B y -> x + y ;                             \
          \  `A x -> 9                                          \
          \}                                                    "
        $ V.pi 3
  , xCont "case `A 1 & `B 2 of {                                \
          \  `A x & `B y & `C z -> 0                            \
          \}                                                    "
  , xCont "case `A 1 & `B 2 of {                                \
          \  `A x & unit -> 0                                   \
          \}                                                    "

  -- Check deep patterns
  , xEval "case `A (`B 1 & `C 2) & `D (`E 4) of {               \
          \  `A (`B x & `C y) -> x + y                          \
          \}                                                    "
        $ V.pi 3
  , xEval "case `A (`B 1 & `C 2) & `D (`E 4) of {               \
          \  (`A `B x) & (`D `E y) -> x + y                     \
          \}                                                    "
        $ V.pi 5
  , xEval "case `A (`B 1 & `C 2) & `D (`E 4) of {               \
          \  (`A (`B x & `C y)) & (`D _) -> x + y               \
          \}                                                    "
        $ V.pi 3
  , xCont "case `A (`B 1 & `C 2) & `D (`E 4) of {               \
          \  `A (`B _ & `Z x) -> x                              \
          \}                                                    "
  , xCont "case `A (`B (`C 0)) of {                             \
          \  `A (`C z) -> z                                     \
          \}                                                    "

  -- Ensure that deep patterns bind the entire content without filtering.
  , xEval "case `A 1 & `B 2 of {                                \
          \  x:`A _ ->                                          \
          \    case x of {                                      \
          \      `A y & `B z -> y + z                           \
          \    }                                                \
          \}                                                    "
        $ V.pi 3

  -- Ensure that outer pattern binders replicate structure rather than the cell
  -- itself.
  , xEval "def x = 1 in                                         \
          \case x of {                                          \
          \  y:int -> x = 2 in y                                \
          \}                                                    "
        $ V.pi 1

  -- Verify pattern checking.
  , xDLbl "case `A 0 of {                                       \
          \  `A x & `A y -> 0                                   \
          \}                                                    "
  , xDBnd "case `A 0 & `B 0 of {                                \
          \  `A x & `B x -> 0                                   \
          \}                                                    "
  , xEval "case `A `A `A 4 of {                                 \
          \  `A `A `A x -> x                                    \
          \}                                                    "
        $ V.pi 4

  -- TODO: we require more unit tests!  (verify inner binder mutation, etc.)
  ]