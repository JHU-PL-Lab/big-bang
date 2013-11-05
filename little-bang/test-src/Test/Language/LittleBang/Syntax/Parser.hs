{-
  This module tests the TinyBangNested Parser.
-}
module Test.Language.LittleBang.Syntax.Parser
( parserTests
) where

import Debug.Trace

import Language.LittleBang.Syntax.Parser
import Language.LittleBang.Syntax.Lexer
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Display (render, makeDoc)
import Test.HUnit

-- | Utility functions for Lexer unit tests:

-- | Display unit tests?
verbose :: Bool
verbose = True

-- | Function for automating calls to lexTinyBangNested and unwrapping result
testContext :: ParserContext
testContext = ParserContext UnknownDocument "UnitTestDoc"

-- | Takes a label, input, expected ouput and generates a TestCase for these values
genUnitTest :: String -> String -> String -> Test
genUnitTest label input expected = 
  if verbose && (not boolAnswer) 
    then trace ("Test " ++ label ++ " parsed\n" ++ result ++ "\nInstead of\n" ++ expected ++ "\n") $ testCase
    else testCase
      where
       result = render $ makeDoc $ parseLittleBang testContext =<< lexLittleBang "" input
       boolAnswer = result == expected
       testCase = TestCase $ assertBool label boolAnswer

parserTests :: Test
parserTests = TestList
  [ testDef
  , testArithOpAssoc
  , testOnionAssoc
  , testApplAssoc
  , testApplPrec
  , testLabelNested
  , testOnionOp
  , testScape
  , testScapeNested
  , testNestedPattern
  , testComplex1
  ]

-- | Test def expressions
testDef :: Test
testDef = genUnitTest "def expression" "def x = 1 in x" "def x = (1) in (x)"

-- | Test arithop associativity
testArithOpAssoc :: Test
testArithOpAssoc = genUnitTest "arith op associativity" "a1 + b2 + c3" "((a1) + (b2)) + (c3)"

-- | Test onion associativity
testOnionAssoc :: Test
testOnionAssoc = genUnitTest "onion associativity" "x & 2 & `A 1" "((x) & (2)) & ((`A 1))"

-- | Test label nested
testLabelNested :: Test
testLabelNested = genUnitTest "nested label " "`A `B `C x" "(`A (`B (`C x)))"

-- | Test application associativity
testApplAssoc :: Test
testApplAssoc = genUnitTest "application associativity" "a b c d" "(((a) apply (b)) apply (c)) apply (d)"

-- | Test application precedence
testApplPrec :: Test
testApplPrec = genUnitTest "application precedence" "a + b & c d" "(a) + ((b) & ((c) apply (d)))"

-- | Test onion op assoc + precedence
testOnionOp :: Test
testOnionOp = genUnitTest "onion op" "`A 1 &- fun &. int" "(((`A 1) &- fun) &. int)"

-- | Test scape
testScape :: Test
testScape = genUnitTest "scape" "v: `A x : () -> x + x" "(v:(`A x:())) -> ((x) + (x))"

-- | Test nested scape
testScapeNested :: Test
testScapeNested = genUnitTest "scape nested" "v: `A x : () -> x & (v2 : `B y : () -> y + x)" "(v:(`A x:())) -> ((x) & ((v2:(`B y:())) -> ((y) + (x))))"

-- | Test nested pattern
testNestedPattern :: Test
testNestedPattern = genUnitTest "nested pattern" "v1: `A x:fun & (`B y:int & `C z:char) -> x + y + z" "(v1:((`A x:fun) &pat ((`B y:int) &pat (`C z:char)))) -> (((x) + (y)) + (z))"

-- | Test complex : default arguments example
complex1String :: String
complex1String = " def obj = seal ((v: `add y1:(`x x:() & `y y:()) -> x + y) & (v2:`sub y2:(`x x:() & `y y:()) -> x - y) ) in def dflt = obj:() ->  obj & (v3: `add a:() -> obj (`add (`x 1 & a))) in def obj2 = dflt obj in obj2 (`add (`y 3))"
complex1Expected :: String
complex1Expected = "def obj = ((seal) apply (((v:(`add y1:((`x x:()) &pat (`y y:())))) -> ((x) + (y))) & ((v2:(`sub y2:((`x x:()) &pat (`y y:())))) -> ((x) - (y))))) in (def dflt = ((obj:()) -> ((obj) & ((v3:(`add a:())) -> ((obj) apply ((`add ((`x 1)) & (a))))))) in (def obj2 = ((dflt) apply (obj)) in ((obj2) apply ((`add (`y 3))))))"
testComplex1 :: Test
testComplex1 = genUnitTest "complex1 : default arguments" complex1String complex1Expected
