module Main where

import qualified Language.BigBang.Syntax.LexerTest as TL
import qualified Language.BigBang.Syntax.ParserTest as TP
import qualified Language.BigBang.Interpreter.InterpreterTest as TI
import qualified Language.BigBang.Render.PrettyPrintTest as TPP
import Test.HUnit

tests :: Test
tests = TestList [TL.tests,TP.tests, TI.tests, TPP.tests]

main :: IO Counts
main = runTestTT tests

