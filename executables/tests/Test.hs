module Main where

import qualified Language.BigBang.Syntax.LexerTest as TL
import qualified Language.BigBang.Syntax.ParserTest as TP
import qualified Language.BigBang.Interpreter.InterpreterTest as TI
import Test.HUnit (Test(..), runTestTT)

tests = TestList [TL.tests,TP.tests, TI.tests]

main = runTestTT tests

