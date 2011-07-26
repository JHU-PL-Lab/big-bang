module Main where

import qualified Tests.Language.BigBang.Syntax.Lexer as TL
import qualified Tests.Language.BigBang.Syntax.Parser as TP
import Test.HUnit (Test(..), runTestTT)

testCases = TestList [TL.testCases,TP.testCases]

main = runTestTT testCases

