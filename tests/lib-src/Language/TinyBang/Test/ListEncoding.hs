module Language.TinyBang.Test.ListEncoding
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import Language.TinyBang.Test.SourceUtils
import qualified Language.TinyBang.Test.ValueUtils as V
import Text.Printf (printf)

srcMakeList :: [Integer] -> TinyBangCode
srcMakeList = foldr addNode "`nil ()"
  where addNode int tbCode =
          printf "`hd %d & (`tl (%s))" int tbCode

srcSum = "fun this -> fun xs ->"
         ++ " case xs of "
         ++ " { `nil junk -> 0 ;"
         ++ "   `hd a -> case xs of"
         ++ "       {`tl b -> [+] a (this b)}"
         ++ " }"

tests :: (?debug :: Bool) => Test
tests = TestLabel "List encoding tests" $ TestList
  [ xEval (srcMultiAppl [srcY, srcSum, srcMakeList [1,2,3]]) $
          V.pi 6
  ]