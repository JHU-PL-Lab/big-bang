module LexerTest where

import Language.BigBang.Syntax.Lexer( Token(..), lexBigBang )
import Test.HUnit

testEmpty = TestCase $ assertEqual Nothing ( alexGetChar ('', [])  )

main = runTestTT testEmpty
