module Main where

import qualified Tests.Language.BigBang.Syntax.Lexer as TL
import qualified Tests.Language.BigBang.Syntax.Parser as TP
import qualified Tests.Language.BigBang.Interpreter.Interpreter as TI
import Test.HUnit (Test(..), runTestTT)

tests = TestList [TL.tests,TP.tests, TI.tests]

main = runTestTT tests

