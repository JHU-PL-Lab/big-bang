module Language.MicroBang.Test.Case
( tests
)
where

import Language.MicroBang.Test.SourceUtils
import Language.MicroBang.Test.UtilFunctions
import qualified Language.MicroBang.Test.ExpressionUtils as E
  ( false
  )
import Language.MicroBang.Test.ExpressionUtils
  ( varX
  )
import qualified Language.MicroBang.Test.ValueUtils as V
  ( pi
  )
import Language.MicroBang.Test.NameUtils
  ( idX
  , lblTrue
  )

import qualified Language.MicroBang.Ast as A

tests :: (?debug :: Bool) => Test
tests = TestLabel "General case tests" $ TestList
  [ xEval "case 1 of {int -> 0}"
          (V.pi 0)

  -- Test that incomplete case statements result in contradiction
  , xCont "case 1 of {unit -> 0}"
  , xCont "case `A 4 of { `B x -> x }"

  -- ensure that closedness works in case expressions
  , xCont "case x of {int -> 0; unit -> `A ()}"
  , xCont "case 4 of {int -> x}"
  , xCont "case 4 of {int -> () ; unit -> x}"
  ---- Check to make sure that path sensitivity does not exist, as we don't
  ---- expect it to.
  , xCont (srcY ++ "(fun this -> fun v ->                       \
                   \    case v of {                             \
                   \      unit -> 0;                            \
                   \      `A underscore ->                      \
                   \        case v of {                         \
                   \          `B y -> [+] 1 (this y)            \
                   \        }                                   \
                   \    }                                       \
                   \) (`A 0 & `B (`A 0 & `B ()))                ")
  , xEval "case `A 3 & `B () of {                               \
          \  `A i -> i                                          \
          \}                                                    "
        $ V.pi 3

  -- TODO: we require more unit tests!  (verify inner binder mutation, etc.)
  ]
