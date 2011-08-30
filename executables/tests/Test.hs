module Main where

import qualified Language.LittleBang.Interpreter.InterpreterTest as TI
import qualified Language.LittleBang.Render.PrettyPrintTest as TPP
import qualified Language.LittleBang.Syntax.LexerTest as TL
import qualified Language.LittleBang.Syntax.ParserTest as TP
import qualified Language.LittleBang.Types.TypesTest as TT
import Test.HUnit

tests :: Test
tests = TestList [TL.tests,TP.tests, TI.tests, TPP.tests, TT.tests]

main :: IO Counts
main = runTestTT tests

