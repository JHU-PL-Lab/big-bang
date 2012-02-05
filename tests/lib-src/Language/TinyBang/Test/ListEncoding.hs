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

srcSum1 = "fun this -> fun xs ->"
          ++ " case xs of"
          ++ " { `nil junk -> 0 ;"
          ++ "   `hd a -> case xs of"
          ++ "       {`tl b -> [+] a (this b)}"
          ++ " }"

srcSum2 = "fun this -> fun accum -> fun xs ->"
          ++ " case xs of"
          ++ " { `nil junk -> accum ;"
          ++ "   `hd a -> case xs of"
          ++ "       {`tl b -> this ([+] a accum) b}"
          ++ " }"

srcSum3 = "fun this -> fun xs ->"
          ++ " case xs of { `acc accum ->"
          ++ " case xs of"
          ++ " { `nil junk -> accum;"
          ++ "   `hd a -> case xs of"
          ++ "       {`tl b -> this (b & `acc ([+] a accum))}"
          ++ " }}"

srcSum4 = "def accum = 0 in fun this -> fun xs ->"
          ++ " case xs of"
          ++ " { `nil junk -> accum;"
          ++ "   `hd a -> case xs of"
          ++ "       {`tl b -> accum = [+] accum a in this b }"
          ++ " }"

testSum xs = map ($ V.pi $ sum xs)
  [ xEval $ srcMultiAppl
      [srcY, srcSum1, srcMakeList xs]
  , xEval $ srcMultiAppl
      [srcY, srcSum2, "0", srcMakeList xs]
  , xEval $ srcMultiAppl
      [srcY, srcSum3, "`acc 0 & " ++ srcMakeList xs]
  , xEval $ srcMultiAppl
      [srcY, srcSum4, srcMakeList xs]
  ]

tests :: (?debug :: Bool) => Test
tests = TestLabel "List encoding tests" $ TestList $ concat
  [ testSum []
  , testSum [1,2,3]
  , testSum [3,2,1]
  ]