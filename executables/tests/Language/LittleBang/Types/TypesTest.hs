module Language.LittleBang.Types.TypesTest
( tests
) where

import qualified Data.Set as Set 
import Test.HUnit hiding (Label)

import Language.LittleBang.Ast
import qualified Language.LittleBang.Types.Types as T
import qualified Language.LittleBang.Types.TypeInference as I
import qualified Language.LittleBang.Types.Closure as C
import Language.LittleBang.Types.UtilTypes
import Language.LittleBang.Syntax.Parser
import Language.LittleBang.Syntax.Lexer

tests :: Test
tests = TestList [basicCases, primitiveBuiltinCases, caseCases, functionCases, equalityCases, onionCases]

{- 
  A utility function which generates a typechecking result for source code.
  The provided string is lexed, parsed, and typechecked as a Big Bang program.
  This function produces True for a successful typecheck and False for
  failure.
-}
typecheckSourceString :: String -> Bool
typecheckSourceString src = typecheckAst $ parseLittleBang $ lexLittleBang src

typecheckAst :: Expr -> Bool
typecheckAst expr =
    Set.null $ Set.filter isBottom $
        C.calculateClosure $ snd $ I.inferTypeTop expr
    where isBottom (T.Bottom _) = True
          isBottom _            = False

-- Test cases that check almost trivial typechecker functionality
basicCases :: Test
basicCases = TestList [testInt, testChar, testVar, testUnit, testLabel]

testInt :: Test
testInt = TestCase $ assertBool
          "Single integer failed to typecheck" $
          typecheckSourceString "1234567890"

testChar :: Test
testChar = TestCase $ assertBool
           "Single character failed to typecheck" $
           typecheckSourceString "'x'"

testVar :: Test
testVar = TestCase $ assertBool
          "Single variable failed to typecheck" $
          typecheckSourceString "x"

testUnit :: Test
testUnit = TestCase $ assertBool
           "Single unit failed to typecheck" $
           typecheckSourceString "()"

testLabel :: Test
testLabel = TestCase $ assertBool
            "Single label failed to typecheck" $
            typecheckSourceString "`A ()"

-- Test cases that ensure that built-in primitive operations typecheck on their
-- appropriate values

primitiveBuiltinCases :: Test
primitiveBuiltinCases = TestList [testPlusInt, testMinusInt, testCompoundInt, testPlusIntChar, testPlusIntUnit, testPlusChar, testPlusUnit, testMinusIntChar, testMinusIntUnit, testMinusChar, testMinusUnit]

testPlusInt :: Test
testPlusInt = TestCase $ assertBool
              "Addition of integers failed to typecheck" $
              typecheckSourceString "plus 1 2"

testMinusInt :: Test
testMinusInt = TestCase $ assertBool
               "Subtraction of integers failed to typecheck" $
               typecheckSourceString "minus 1 2"

testCompoundInt :: Test
testCompoundInt = TestCase $ assertBool
                  "Compound statement involving integers failed to typecheck" $
                  typecheckSourceString "plus (minus (plus 1 2) 3) (plus (-2) (minus 4 0))"

testPlusIntChar :: Test
testPlusIntChar = TestCase $ assertBool
                  "Addition of integer and char typechecked" $
                  not $ typecheckSourceString "plus 1 'a'"

testPlusIntUnit :: Test
testPlusIntUnit = TestCase $ assertBool
                  "Addition of integer and unit typechecked" $
                  not $ typecheckSourceString "plus 1 ()"

testPlusChar :: Test
testPlusChar = TestCase $ assertBool
               "Addition of characters typechecked" $
               not $ typecheckSourceString "plus 'a' 'a'"

testPlusUnit :: Test
testPlusUnit = TestCase $ assertBool
                 "Addition of units typechecked" $
                 not $ typecheckSourceString "plus () ()"

testMinusIntChar :: Test
testMinusIntChar = TestCase $ assertBool
                   "Subtraction of integer and char typechecked" $
                   not $ typecheckSourceString "minus 1 'a'"

testMinusIntUnit :: Test
testMinusIntUnit = TestCase $ assertBool
                   "Subtraction of integer and unit typechecked" $
                   not $ typecheckSourceString "minus 1 ()"

testMinusChar :: Test
testMinusChar = TestCase $ assertBool
                "Subtraction of characters typechecked" $
                not $ typecheckSourceString "minus 'a' 'a'"

testMinusUnit :: Test
testMinusUnit = TestCase $ assertBool
                "Subtraction of units typechecked" $
                not $ typecheckSourceString "minus () ()"

-- Test cases that ensure that case expressions resolve types correctly
caseCases :: Test
caseCases = TestList [testCaseLabel, testCaseLabelMismatch, testCaseReturnTypeMismatch]

testCaseLabel :: Test
testCaseLabel = TestCase $ assertBool 
                "Casing over label failed to typecheck" $
                typecheckSourceString
                    " case `A 5 of                  \
                    \   { `A x -> x }               "

testCaseLabelMismatch :: Test
testCaseLabelMismatch = TestCase $ assertBool
                        "Casing over mismatched labels typechecked" $
                         not $ typecheckSourceString
                                    "case `A 4 of                    \
                                    \    { `B x -> x }               "

testCaseReturnTypeMismatch :: Test
testCaseReturnTypeMismatch = TestCase $ assertBool
                             "Case with return type mismatch failed to typecheck"
                             (typecheckSourceString
                                 "case x of {     \
                                 \    int -> 0;   \
                                 \    char -> 'a'}")

-- Tests that ensure function applications typecheck correctly
functionCases :: Test
functionCases = TestList [testFunctionDef, testIdentityApplication, testFuncApply, testFuncApplyVar, testFuncApplyMismatch, testSelfApplyApplication, testYCombinator, testYCombinatorAppl]

testFunctionDef :: Test
testFunctionDef = TestCase $ assertBool
                  "Function definition failed to typecheck" $
                  typecheckSourceString "(fun x -> x)"

testIdentityApplication :: Test
testIdentityApplication = TestCase $ assertBool
                          "Application of identity function to itself failed to typecheck" $
                          typecheckSourceString "(fun x -> x) (fun x -> x)"

testFuncApply :: Test
testFuncApply = TestCase $ assertBool
                "\"(fun x -> plus x 1) 1\" failed to typecheck" $
                typecheckSourceString "(fun x -> plus x 1) 1"

testFuncApplyVar :: Test
testFuncApplyVar = TestCase $ assertBool
                   "\"(fun n -> plus n 2) x\" failed to typecheck" $
                   typecheckSourceString "(fun x -> plus n 2)"

testFuncApplyMismatch :: Test
testFuncApplyMismatch = TestCase $ assertBool
                        "\"(fun x -> plus x 1) \'a\'\" typechecked" $
                        not $ typecheckSourceString "(fun x -> plus x 1) 'a'"

testSelfApplyApplication :: Test
testSelfApplyApplication = TestCase $ assertBool
                           "Application of self application to itself failed to typecheck" $
                           typecheckSourceString "(fun x -> x x) (fun x -> x x)"

testYCombinator :: Test
testYCombinator = TestCase $ assertBool
                  "YCombinator failed to typecheck" $
                  typecheckSourceString "fun body -> (fun f -> fun arg -> f f arg) (fun this -> fun arg -> body (this this) arg)"
 

testYCombinatorAppl :: Test
testYCombinatorAppl = TestCase $ assertBool
                  "YCombinator application failed to typecheck" $
                  typecheckSourceString
                    "(fun body -> (fun f -> (fun arg -> f f arg))                       \
                    \             (fun this -> (fun arg -> body (this this) arg)))      \
                    \(fun this -> (fun x -> case equal x 0 of {                         \
                    \                           `True z -> 0;                           \
                    \                           `False z -> plus x (this (minus x 1))   \
                    \                       }))                                         \
                    \ 5                                                                 "

-- Tests that ensure checks for equality typecheck correctly
equalityCases :: Test
equalityCases = TestList [testEqualInt, testEqualChar, testEqualIntChar, testEqualIntUnit, testEqualTrue, testEqualTrueFalse, testEqualIdenticalFunction, testEqualDifferentFunction, testEqualLabel, testEqualLabelMismatch, testEqualUnit]

testEqualInt :: Test
testEqualInt = TestCase $ assertBool
               "Testing for equality on integers failed to typecheck" $
               typecheckSourceString "equal 1 1"

testEqualChar :: Test
testEqualChar = TestCase $ assertBool
                "Testing for equality on characters failed to typecheck" $
                typecheckSourceString "equal 'a' 'a'"

testEqualIntChar :: Test
testEqualIntChar = TestCase $ assertBool
                   "Testing for equality of integer and character typechecked" $
                   not $ typecheckSourceString "equal 1 'a'"

testEqualIntUnit :: Test
testEqualIntUnit = TestCase $ assertBool
                   "Testing for equality of integer and unit typechecked" $
                   not $ typecheckSourceString "equal 1 ()"

testEqualTrue :: Test
testEqualTrue = TestCase $ assertBool
                   "Testing equality with true failed to typecheck" $
                   typecheckSourceString "equal (`True ()) (`True ())"

testEqualTrueFalse :: Test
testEqualTrueFalse = TestCase $ assertBool
                   "Testing equality with true and false typechecked" $
                   not $ typecheckSourceString "equal (`True ()) (`False ())"

testEqualIdenticalFunction :: Test
testEqualIdenticalFunction = TestCase $ assertBool
                    "Testing equality of identical functions failed to typecheck" $
                    typecheckSourceString "(fun f -> equal f f) (fun x -> x)"

testEqualDifferentFunction :: Test
testEqualDifferentFunction = TestCase $ assertBool
                    "Testing equality of non-identical functions typechecked" $
                    not $ typecheckSourceString "equal (fun x -> x) (fun y -> y)"

testEqualLabel :: Test
testEqualLabel = TestCase $ assertBool
                 "Testing equality of labels failed to typecheck" $
                 typecheckSourceString "equal (`A ()) (`A ())"

testEqualLabelMismatch :: Test
testEqualLabelMismatch = TestCase $ assertBool
                         "Testing equality for mismatched labels typechecked" $
                         not $ typecheckSourceString "equal (`A ()) (`B ())"

testEqualUnit :: Test
testEqualUnit = TestCase $ assertBool
                "Testing equality of unit failed to typecheck" $
                typecheckSourceString "equal () ()"

-- Tests that ensure onions typecheck correctly
onionCases :: Test
onionCases = TestList [testOnionPlus, testOnionPlusNoInt, testOnionOnionPlus, testCompoundOnionPlus, testSubOnionPlus, testOnionFuncApply]

testOnionPlus :: Test
testOnionPlus = TestCase $ assertBool
                "Adding onion with integer component and integer failed to typecheck" $
                typecheckSourceString "plus (2 & 'b') 2"

testOnionPlusNoInt :: Test
testOnionPlusNoInt = TestCase $ assertBool
                     "Adding onion with no integer component and integer typechecked" $
                     not $ typecheckSourceString "plus (`True () & 'z') 2"

testOnionOnionPlus :: Test
testOnionOnionPlus = TestCase $ assertBool
                     "Adding two onions with integer components failed to typecheck" $
                     typecheckSourceString "plus (2 & 'x') ('y' & 2)"

testCompoundOnionPlus :: Test
testCompoundOnionPlus = TestCase $ assertBool
                        "Adding compound onions with integer components failed to typecheck" $
                        typecheckSourceString "plus (2 & ('a' & ())) ((2 & 'b') & ())"

testSubOnionPlus :: Test
testSubOnionPlus = TestCase $ assertBool
                   "Adding onion and subonion with integers components failed to typecheck" $
                   typecheckSourceString "plus (1 & ('a' & ())) ('a' & (1 & ()))"

testOnionFuncApply :: Test
testOnionFuncApply = TestCase $ assertBool
                    "Application of function component within onion failed to typecheck" $
                    typecheckSourceString "(1 & (fun x -> x)) 1"
