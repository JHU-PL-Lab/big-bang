module Language.LittleBang.Syntax.LexerTest
( tests
) where

import Language.LittleBang.Syntax.Lexer
import Test.HUnit hiding (Label)

tests :: Test
tests = TestList [individualTokenCases, simplePrograms, testNoSpaces, edgeCases]

-- TODO: should be replaced with expectation-based tests
doLex :: String -> [Token]
doLex src = case lexLittleBang src of
    Left err -> error err
    Right tokens -> tokens

-- Test cases that check lexing of individual tokens is correct
individualTokenCases :: Test
individualTokenCases = TestList [testPositiveIntegerLiteral, testNegativeIntegerLiteral, testCharLiteral, testLambda, testArrow, testLabelPrefix, testFun, testCase, testOf, testInt, testChar, testUnit, testOnionCons, testOpenParen, testCloseParen, testOpenBlock, testCloseBlock, testSeparator]

testPositiveIntegerLiteral :: Test
testPositiveIntegerLiteral = TestCase $ assertEqual
  "Test with input 1234567890"
  [TokIntegerLiteral 1234567890]
  (doLex "1234567890")

testNegativeIntegerLiteral :: Test
testNegativeIntegerLiteral = TestCase $ assertEqual
  "Test with input -1234567890"
  [TokIntegerLiteral (-1234567890)]
  (doLex "-1234567890")

testCharLiteral :: Test
testCharLiteral = TestCase $ do
  assertEqual
      "Test 'a'"
      [TokCharLiteral 'a']
      (doLex "'a'")
  assertEqual
      "Test 'A'"
      [TokCharLiteral 'A']
      (doLex "'A'")
  assertEqual
      "Test 'z'"
      [TokCharLiteral 'z']
      (doLex "'z'")
  assertEqual
      "Test 'Z'"
      [TokCharLiteral 'Z']
      (doLex "'Z'")

testLambda :: Test
testLambda = TestCase $ assertEqual
  "Test \\ treated as lambda"
  [TokLambda]
  (doLex "\\")

testArrow :: Test
testArrow = TestCase $ assertEqual
  "Test -> treated as arrow"
  [TokArrow]
  (doLex "->")

testLabelPrefix :: Test
testLabelPrefix = TestCase $ assertEqual
  "Test ` treated as label prefix"
  [TokLabelPrefix]
  (doLex "`")

testFun :: Test
testFun = TestCase $ assertEqual
  "Test fun"
  [TokFun]
  (doLex "fun")

testCase :: Test
testCase = TestCase $ assertEqual
  "Test case"
  [TokCase]
  (doLex "case")

testOf :: Test
testOf = TestCase $ assertEqual
  "Test of"
  [TokOf]
  (doLex "of")

testInt :: Test
testInt = TestCase $ assertEqual
  "Test int"
  [TokInteger]
  (doLex "int")

testChar :: Test
testChar =  TestCase $ assertEqual
  "Test char"
  [TokChar]
  (doLex "char")

testUnit :: Test
testUnit = TestCase $ assertEqual
  "Test unit"
  [TokUnit]
  (doLex "unit")

testOnionCons :: Test
testOnionCons = TestCase $ assertEqual
  "Test & treated as Onion cons"
  [TokOnionCons]
  (doLex "&")

testOpenParen :: Test
testOpenParen = TestCase $ assertEqual
  "Test ("
  [TokOpenParen]
  (doLex "(")

testCloseParen :: Test
testCloseParen = TestCase $ assertEqual
  "Test )"
  [TokCloseParen]
  (doLex ")")

testOpenBlock :: Test
testOpenBlock = TestCase $ assertEqual
  "Test { treated as open block"
  [TokOpenBlock]
  (doLex "{")

testCloseBlock :: Test
testCloseBlock = TestCase $ assertEqual
  "Test } treated as close block"
  [TokCloseBlock]
  (doLex "}")

testSeparator :: Test
testSeparator = TestCase $ assertEqual
  "Test ; treated as separator"
  [TokSeparator]
  (doLex ";")


-- Test cases that check lexing of simple programs
simplePrograms :: Test
simplePrograms = TestList [testString, testMap, testNewFun1, testNewFun2, testCaseDef, testPerverse]

testString :: Test
testString = TestCase $ assertEqual
  "Test input of \"'s''t''r''i''n''g'\""
  [TokCharLiteral 's', TokCharLiteral 't', TokCharLiteral 'r', TokCharLiteral 'i', TokCharLiteral 'n', TokCharLiteral 'g']
  (doLex "'s''t''r''i''n''g'")

testMap :: Test
testMap = TestCase $ assertEqual
  "map (\\x -> x) (1;2;3)"
  [TokIdentifier "map", TokOpenParen, TokLambda, TokIdentifier "x", TokArrow, TokIdentifier "x", TokCloseParen, TokOpenParen, TokIntegerLiteral 1, TokSeparator, TokIntegerLiteral 2, TokSeparator, TokIntegerLiteral 3, TokCloseParen]
  (doLex "map (\\x -> x) (1;2;3)")

testNewFun1 :: Test
testNewFun1 = TestCase $ assertEqual
  "fun double x ->  plus x x"
  [TokFun, TokIdentifier "double", TokIdentifier "x", TokArrow, TokIdentifier "plus", TokIdentifier "x", TokIdentifier "x"]
  (doLex "fun double x -> plus x x")

testNewFun2 :: Test
testNewFun2 = TestCase $ assertEqual
  "fun abs x -> if x GTE 0 then x else negative x"
  [TokFun, TokIdentifier "abs", TokIdentifier "x", TokArrow, TokIdentifier "if", TokIdentifier "x", TokIdentifier "GTE", TokIntegerLiteral 0, TokIdentifier "then", TokIdentifier "x", TokIdentifier "else", TokIdentifier "negative", TokIdentifier "x"]
 (doLex "fun abs x -> if x GTE 0 then x else negative x")

testCaseDef :: Test
testCaseDef = TestCase $ assertEqual
  "case val of\n  x -> x"
  [TokCase, TokIdentifier "val", TokOf, TokIdentifier "x", TokArrow, TokIdentifier "x"]
  (doLex "case val of\ 
              \    x -> x")

testPerverse :: Test
testPerverse = TestCase $ assertEqual
  "{`(int & unit} & char)"
  [TokOpenBlock, TokLabelPrefix, TokOpenParen, TokInteger, TokOnionCons, TokUnit, TokCloseBlock, TokOnionCons, TokChar, TokCloseParen]
  (doLex "{`(int & unit} & char)")


-- Variations of the simple programs tests without spaces
testNoSpaces :: Test
testNoSpaces = TestList [testLambdaNoSpaces, testOnionConsNoSpaces, testUnitNoSpaces, testPerverseNoSpaces1, testPerverseNoSpaces2, testPerverseNoSpaces3, testPerverseNoSpaces4, testPerverseNoSpaces5]

testLambdaNoSpaces :: Test
testLambdaNoSpaces =  TestCase $ assertEqual
  "Variation of lambda test with no spaces"
  [TokOpenParen, TokLambda, TokIdentifier "x", TokArrow, TokIdentifier "x", TokCloseParen]
  (doLex "(\\x->x)")

testOnionConsNoSpaces :: Test
testOnionConsNoSpaces = TestCase $ assertEqual
  "Variation of onion cons test with no spaces"
  [TokIdentifier "a", TokOnionCons, TokOpenParen, TokIdentifier "b", TokOnionCons, TokIdentifier "c", TokCloseParen]
  (doLex "a&(b&c)")

testUnitNoSpaces :: Test
testUnitNoSpaces = TestCase $ assertEqual
  "Variation of unit test with no spaces"
  [TokLabelPrefix, TokIdentifier "True", TokOpenParen, TokCloseParen]
  (doLex "`True()")

testPerverseNoSpaces1 :: Test
testPerverseNoSpaces1 = TestCase $ assertEqual
  "Variation of perverse test with no spaces"
  [TokOpenParen, TokIntegerLiteral 1, TokSeparator, TokIdentifier "xyz", TokSeparator, TokOpenBlock, TokCharLiteral 'c', TokSeparator, TokCloseParen, TokCloseBlock]
  (doLex "(1;xyz;{\'c\';)}")

testPerverseNoSpaces2 :: Test
testPerverseNoSpaces2 = TestCase $ assertEqual
  "Test lexing of \"1\'c\'xyz\""
  [TokIntegerLiteral 1, TokCharLiteral 'c', TokIdentifier "xyz"]
  (doLex "1\'c\'xyz")

testPerverseNoSpaces3 :: Test
testPerverseNoSpaces3 = TestCase $ assertEqual
  "Test lexing of \"1xyz\'c\'\""
  [TokIntegerLiteral 1, TokIdentifier "xyz\'c\'"]
  (doLex "1xyz\'c\'")

testPerverseNoSpaces4 :: Test
testPerverseNoSpaces4 = TestCase $ assertEqual
  "Test lexing of \"xyz\'c\'1\""
  [TokIdentifier "xyz\'c\'1"]
  (doLex "xyz\'c\'1")

testPerverseNoSpaces5 :: Test
testPerverseNoSpaces5 = TestCase $ assertEqual
  "Test lexing of \"xyz1\'c\'\""
  [TokIdentifier "xyz1\'c\'"]
  (doLex "xyz1\'c\'")

-- Test cases that handle edge cases for lexer input
edgeCases :: Test
edgeCases = TestList [testEmptyString, testWhitespace, testNegativeZero]

testEmptyString :: Test
testEmptyString = TestCase $ assertBool 
  "Should get [] from an empty string" 
  ( null $ doLex "" )

testWhitespace :: Test
testWhitespace = TestCase $ do
  assertBool
      "Ignore spaces"
      (null $ doLex " ")
  assertBool
      "Ignore tabs"
      (null $ doLex "\t")

testNegativeZero :: Test
testNegativeZero = TestCase $ assertEqual
  "Test handling input of -0"
  [TokIntegerLiteral 0]
  (doLex "-0")
