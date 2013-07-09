{-
  This module tests the TinyBangNested Parser.
-}
module Test.Language.TinyBangNested.Syntax.Parser
( parserTests
) where


import Debug.Trace

import Language.TinyBangNested.Syntax.Parser
import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBang.Syntax.Location
import Language.TinyBangNested.Ast.Data
import Language.TinyBang.Display (render, makeDoc)
import Language.TinyBang.Ast.Data hiding (Expr)

import Test.HUnit

-- | Utility functions for Lexer unit tests:

-- | Display unit tests?
verbose :: Bool
verbose = False

-- | Function for automating calls to lexTinyBangNested and unwrapping result
testContext :: ParserContext
testContext = ParserContext UnknownDocument "UnitTestDoc"

-- | Runs the parser on a stream of PositiontalTokens, producing an Expr or failing
getParserResult :: [PositionalToken] -> Expr
getParserResult input =  extractExpr $ parseTinyBangNested testContext input

-- | Prints a token stream
printTokenStream :: [PositionalToken] -> String
printTokenStream tkst = concat $ map printTok tkst
			 where
		          printTok :: PositionalToken -> String 
			  printTok (PositionalToken _ _ posTok) = show posTok  ++ " "

-- | Takes a label, input, expected ouput and generates a TestCase for these values
genUnitTest :: String -> String -> String -> Test
genUnitTest label input expected = if verbose then trace ( result ++ "\n" ++ ( printTokenStream tokenStream )) $ testCase else testCase 
                                    where
					tokenStream = extractRight $ lexTinyBangNested ("Testing " ++ label) input
					result = render $ makeDoc $ getParserResult tokenStream
                                        testCase = TestCase $ assertBool label $ result == expected

extractExpr :: Either String Expr -> Expr
extractExpr (Left x) = error ("parse failed: " ++ x)
extractExpr (Right x) = x

extractRight :: Either a b -> b
extractRight (Left _ ) = error "left error!"
extractRight (Right x ) = x

parserTests :: Test

parserTests = TestList [testDef, testArithOpAssoc, testOnionAssoc, testApplAssoc, testApplPrec, testLabelNested, testOnionOp, testScape, testScapeNested, testNestedPattern, testComplex1]

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
testScape = genUnitTest "scape" "`A x : () -> x + x" "((`A x:())) -> ((x) + (x))"

-- | Test nested scape
testScapeNested :: Test
testScapeNested = genUnitTest "scape nested" "`A x : () -> x & (`B y : () -> y + x)" "((`A x:())) -> ((x) & (((`B y:())) -> ((y) + (x))))"

-- | Test nested pattern
testNestedPattern :: Test
testNestedPattern = genUnitTest "nested pattern" "`A x:fun & (`B y:int & `C z:char) -> x + y + z" "(((`A x:fun) &pat ((`B y:int) &pat (`C z:char)))) -> (((x) + (y)) + (z))"

-- | Test complex : default arguments example
complex1String :: String
complex1String = " def obj = seal ((`add y1:(`x x:() & `y y:()) -> x + y) & (`sub y2:(`x x:() & `y y:()) -> x - y) ) in def dflt = obj:() ->  obj & (`add a:() -> obj (`add (`x 1 & a))) in def obj2 = dflt obj in obj2 (`add (`y 3))"
complex1Expected :: String
complex1Expected = "def obj = ((seal) apply ((((`add y1:((`x x:()) &pat (`y y:())))) -> ((x) + (y))) & (((`sub y2:((`x x:()) &pat (`y y:())))) -> ((x) - (y))))) in (def dflt = ((obj:()) -> ((obj) & (((`add a:())) -> ((obj) apply ((`add ((`x 1)) & (a))))))) in (def obj2 = ((dflt) apply (obj)) in ((obj2) apply ((`add (`y 3))))))"
testComplex1 :: Test
testComplex1 = genUnitTest "complex1 : default arguments" complex1String complex1Expected
