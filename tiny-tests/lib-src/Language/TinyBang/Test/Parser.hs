module Language.TinyBang.Test.Parser
( tests
)
where

import Language.TinyBang.Test.UtilFunctions
import qualified Language.TinyBang.Config as Cfg

tests :: (?conf :: Cfg.Config) => Test
tests = TestLabel "Miscellaneous parser tests" $ TestList
  [ fPars ""
  , fPars "(expr"
  , fPars "square x;"
  , fPars "case x of { int -> 3; }"
  , fPars "case x of {}"
  , fPars "def x"
  , fPars "def x in y"
  , fPars "x = y"
  , fPars "{ 1 }"
  ]
