module Tests.Language.BigBang.Syntax.Lexer
( tests
) where

import Language.BigBang.Syntax.Lexer
import Test.HUnit

edgeCases = TestLabel "Edge cases" ( TestList [testEmptyString, testWhitespace, testNegativeZero] )

testEmptyString = TestCase $ assertBool 
  "Should get [] from an empty string" 
  ( null $ lexBigBang "" )

testWhitespace = TestCase $ do
  assertBool
      "Ignore spaces"
      (null $ lexBigBang " ")
  assertBool
      "Ignore tabs"
      (null $ lexBigBang "\t")

testNegativeZero = TestCase $ assertEqual
  "Test handling input of -0"
  [TokIntegerLiteral 0]
  (lexBigBang "-0")

individualTokenCases = TestLabel "Individual tokens" ( TestList [testNegativeIntegerLiteral, testPositiveIntegerLiteral, testLambda, testArrow, testLabelPrefix, testFun, testCase, testOnionCons, testOpenParen, testCloseParen, testOpenBlock, testCloseBlock, testSeparator, testOf, testInt, testChar, testUnit, testCharLiteral] )

testPositiveIntegerLiteral = TestCase $ assertEqual
  "Test with input 1234567890"
  [TokIntegerLiteral 1234567890]
  (lexBigBang "1234567890")


testNegativeIntegerLiteral = TestCase $ assertEqual
  "Test with input -1234567890"
  [TokIntegerLiteral (-1234567890)]
  (lexBigBang "-1234567890")

testCharLiteral = TestCase $ do
  assertEqual
      "Test 'a'"
      [TokCharLiteral 'a']
      (lexBigBang "'a'")
  assertEqual
      "Test 'A'"
      [TokCharLiteral 'A']
      (lexBigBang "'A'")
  assertEqual
      "Test 'z'"
      [TokCharLiteral 'z']
      (lexBigBang "'z'")
  assertEqual
      "Test 'Z'"
      [TokCharLiteral 'Z']
      (lexBigBang "'Z'")

testLambda = TestCase $ assertEqual
  "Test \\ treated as lambda"
  [TokLambda]
  (lexBigBang "\\")

testArrow = TestCase $ assertEqual
  "Test -> treated as arrow"
  [TokArrow]
  (lexBigBang "->")

testLabelPrefix = TestCase $ assertEqual
  "Test ` treated as label prefix"
  [TokLabelPrefix]
  (lexBigBang "`")

testFun = TestCase $ assertEqual
  "Test fun"
  [TokFun]
  (lexBigBang "fun")

testCase = TestCase $ assertEqual
  "Test case"
  [TokCase]
  (lexBigBang "case")

testOf = TestCase $ assertEqual
  "Test of"
  [TokOf]
  (lexBigBang "of")

testInt = TestCase $ assertEqual
  "Test int"
  [TokInteger]
  (lexBigBang "int")

testChar =  TestCase $ assertEqual
  "Test char"
  [TokChar]
  (lexBigBang "char")

testUnit = TestCase $ assertEqual
  "Test unit"
  [TokUnit]
  (lexBigBang "unit")

testOnionCons = TestCase $ assertEqual
  "Test & treated as Onion cons"
  [TokOnionCons]
  (lexBigBang "&")

testOpenParen = TestCase $ assertEqual
  "Test ("
  [TokOpenParen]
  (lexBigBang "(")

testCloseParen = TestCase $ assertEqual
  "Test )"
  [TokCloseParen]
  (lexBigBang ")")

testOpenBlock = TestCase $ assertEqual
  "Test { treated as open block"
  [TokOpenBlock]
  (lexBigBang "{")

testCloseBlock = TestCase $ assertEqual
  "Test } treated as close block"
  [TokCloseBlock]
  (lexBigBang "}")

testSeparator = TestCase $ assertEqual
  "Test ; treated as separator"
  [TokSeparator]
  (lexBigBang ";")

simplePrograms = TestList [testString, testMap, testNewFun2, testNewFun2, testCaseDef, testPerverse]

testString = TestCase $ assertEqual
  "Test input of \"'s''t''r''i''n''g'\""
  [TokCharLiteral 's', TokCharLiteral 't', TokCharLiteral 'r', TokCharLiteral 'i', TokCharLiteral 'n', TokCharLiteral 'g']
  (lexBigBang "'s''t''r''i''n''g'")

testMap = TestCase $ assertEqual
  "map (\\x -> x) (1;2;3)"
  [TokIdentifier "map", TokOpenParen, TokLambda, TokIdentifier "x", TokArrow, TokIdentifier "x", TokCloseParen, TokOpenParen, TokIntegerLiteral 1, TokSeparator, TokIntegerLiteral 2, TokSeparator, TokIntegerLiteral 3, TokCloseParen]
  (lexBigBang "map (\\x -> x) (1;2;3)")

testNewFun1 = TestCase $ assertEqual
  "fun double x ->  plus x x"
  [TokFun, TokIdentifier "double", TokIdentifier "x", TokArrow, TokIdentifier "plus", TokIdentifier "x", TokIdentifier "x"]
  (lexBigBang "fun double x -> plus x x")

testNewFun2 = TestCase $ assertEqual
  "fun abs x -> if x GTE 0 then x else negative x"
  [TokFun, TokIdentifier "abs", TokIdentifier "x", TokArrow, TokIdentifier "if", TokIdentifier "x", TokIdentifier "GTE", TokIntegerLiteral 0, TokIdentifier "then", TokIdentifier "x", TokIdentifier "else", TokIdentifier "negative", TokIdentifier "x"]
 (lexBigBang "fun abs x -> if x GTE 0 then x else negative x")

testCaseDef = TestCase $ assertEqual
  "case val of\n  x -> x"
  [TokCase, TokIdentifier "val", TokOf, TokIdentifier "x", TokArrow, TokIdentifier "x"]
  (lexBigBang "case val of\n  x -> x")

testPerverse = TestCase $ assertEqual
  "{`(int & unit} & char)"
  [TokOpenBlock, TokLabelPrefix, TokOpenParen, TokInteger, TokOnionCons, TokUnit, TokCloseBlock, TokOnionCons, TokChar, TokCloseParen]
  (lexBigBang "{`(int & unit} & char)")

testVariations = TestList [testPerverseNoSpaces2, testPerverseNoSpaces3, testPerverseNoSpaces4, testPerverseNoSpaces5, testPerverseNoSpaces, testUnitNoSpaces, testLambdaNoSpaces, testOnionConsNoSpaces]

testLambdaNoSpaces =  TestCase $ assertEqual
  "Variation of lambda test with no spaces"
  [TokOpenParen, TokLambda, TokIdentifier "x", TokArrow, TokIdentifier "x", TokCloseParen]
  (lexBigBang "(\\x->x)")

testOnionConsNoSpaces = TestCase $ assertEqual
  "Variation of onion cons test with no spaces"
  [TokIdentifier "a", TokOnionCons, TokOpenParen, TokIdentifier "b", TokOnionCons, TokIdentifier "c", TokCloseParen]
  (lexBigBang "a&(b&c)")

testUnitNoSpaces = TestCase $ assertEqual
  "Variation of unit test with no spaces"
  [TokLabelPrefix, TokIdentifier "True", TokOpenParen, TokCloseParen]
  (lexBigBang "`True()")

testPerverseNoSpaces = TestCase $ assertEqual
  "Variation of perverse test with no spaces"
  [TokOpenParen, TokIntegerLiteral 1, TokSeparator, TokIdentifier "xyz", TokSeparator, TokOpenBlock, TokCharLiteral 'c', TokSeparator, TokCloseParen, TokCloseBlock]
  (lexBigBang "(1;xyz;{\'c\';)}")

testPerverseNoSpaces2 = TestCase $ assertEqual
  "Test lexing of \"1\'c\'xyz\""
  [TokIntegerLiteral 1, TokCharLiteral 'c', TokIdentifier "xyz"]
  (lexBigBang "1\'c\'xyz")

testPerverseNoSpaces3 = TestCase $ assertEqual
  "Test lexing of \"1xyz\'c\'\""
  [TokIntegerLiteral 1, TokIdentifier "xyz\'c\'"]
  (lexBigBang "1xyz\'c\'")

testPerverseNoSpaces4 = TestCase $ assertEqual
  "Test lexing of \"xyz\'c\'1\""
  [TokIdentifier "xyz\'c\'1"]
  (lexBigBang "xyz\'c\'1")

testPerverseNoSpaces5 = TestCase $ assertEqual
  "Test lexing of \"xyz1\'c\'\""
  [TokIdentifier "xyz1\'c\'"]
  (lexBigBang "xyz1\'c\'")

tests = TestList [edgeCases, individualTokenCases, simplePrograms, testVariations]
