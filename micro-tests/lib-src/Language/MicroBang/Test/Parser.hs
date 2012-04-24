module Language.MicroBang.Test.Parser
( tests
)
where

import Language.MicroBang.Test.UtilFunctions

tests :: (?conf :: Bool) => Test
tests = TestLabel "Miscellaneous parser tests" $ TestList
  [ fPars ""
  , fPars "(expr"
  , fPars "square x;"
  , fPars "case x of { int -> 3; }"
  , fPars "case x of {}"
  , fPars "{ 1 }"
  ]