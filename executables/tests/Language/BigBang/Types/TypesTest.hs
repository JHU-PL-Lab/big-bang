module Language.BigBang.Types.TypesTest
( tests
) where

import qualified Data.Map as Map 

import qualified Data.Set as Set 
import Test.HUnit hiding (Label)

import Language.BigBang.Ast
import qualified Language.BigBang.Types.Types as T
import qualified Language.BigBang.Types.TypeInference as I
import qualified Language.BigBang.Types.Closure as C
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Syntax.Parser
import Language.BigBang.Syntax.Lexer

tests :: Test
tests = TestList [basicCases, primitiveBuiltinCases, caseCases, functionCases, equalityCases, onionCases]

{- 
  A utility function which generates a typechecking result for source code.
  The provided string is lexed, parsed, and typechecked as a Big Bang program.
  This function produces True for a successful typecheck and False for
  failure.
-}
typecheckSourceString :: String -> Bool
typecheckSourceString src = typecheckAst $ parseBigBang $ lexBigBang src

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
          "Single integer failed to typecheck"
          (typecheckAst (PrimInt 1234567890))

testChar :: Test
testChar = TestCase $ assertBool
           "Single character failed to typecheck"
           (typecheckAst (PrimChar 'x'))

testVar :: Test
testVar = TestCase $ assertBool
          "Single variable failed to typecheck"
          (typecheckAst (Var (ident "x")))

testUnit :: Test
testUnit = TestCase $ assertBool
           "Single unit failed to typecheck"
           (typecheckAst PrimUnit)

testLabel :: Test
testLabel = TestCase $ assertBool
            "Single label failed to typecheck"
            (typecheckAst (Label (labelName "A") PrimUnit))

-- Test cases that ensure that built-in primitive operations typecheck on their
-- appropriate values

primitiveBuiltinCases :: Test
primitiveBuiltinCases = TestList [testPlusInt, testMinusInt, testCompoundInt, testPlusIntChar, testPlusIntUnit, testPlusChar, testPlusUnit, testMinusIntChar, testMinusIntUnit, testMinusChar, testMinusUnit]

testPlusInt :: Test
testPlusInt = TestCase $ assertBool
              "Addition of integers failed to typecheck"
               (typecheckAst (Plus (PrimInt 1) (PrimInt 2)))

testMinusInt :: Test
testMinusInt = TestCase $ assertBool
               "Subtraction of integers failed to typecheck" 
               (typecheckAst (Minus (PrimInt 1) (PrimInt 2)))

testCompoundInt :: Test
testCompoundInt = TestCase $ assertBool
                  "Compound statement involving integers failed to typecheck"  
                  (typecheckAst
                      (Plus
                          (Minus
                              (Plus
                                  (PrimInt 1)
                                  (PrimInt 2))
                                  (PrimInt 3))
                             (Plus
                                 (PrimInt (-2))
                             (Minus
                                 (PrimInt 4)
                                 (PrimInt 0)))))

testPlusIntChar :: Test
testPlusIntChar = TestCase $ assertBool
                  "Addition of integer and char typechecked"
                  (not $ typecheckAst (Plus (PrimInt 1) (PrimChar 'a')))

testPlusIntUnit :: Test
testPlusIntUnit = TestCase $ assertBool
                  "Addition of integer and unit typechecked"
                  (not $ typecheckAst (Plus (PrimInt 1) PrimUnit))

testPlusChar :: Test
testPlusChar = TestCase $ assertBool
               "Addition of characters typechecked"
               (not $ typecheckAst (Plus (PrimChar 'a') (PrimChar 'a')))

testPlusUnit :: Test
testPlusUnit = TestCase $ assertBool
                 "Addition of units typechecked"
                 (not $ typecheckAst (Plus PrimUnit PrimUnit))

testMinusIntChar :: Test
testMinusIntChar = TestCase $ assertBool
                   "Subtraction of integer and char typechecked"
                   (not $ typecheckAst (Minus (PrimInt 1) (PrimChar 'a')))

testMinusIntUnit :: Test
testMinusIntUnit = TestCase $ assertBool
                   "Subtraction of integer and unit typechecked"
                   (not $ typecheckAst (Minus (PrimInt 1) PrimUnit))

testMinusChar :: Test
testMinusChar = TestCase $ assertBool
                "Subtraction of characters typechecked"
                (not $ typecheckAst (Minus (PrimChar 'a') (PrimChar 'a')))

testMinusUnit :: Test
testMinusUnit = TestCase $ assertBool
                "Subtraction of units typechecked"
                (not $ typecheckAst (Minus PrimUnit PrimUnit))

-- Test cases that ensure that case expressions resolve types correctly
caseCases :: Test
caseCases = TestList [testCaseLabel, testCaseLabelMismatch, testCaseReturnTypeMismatch]

testCaseLabel :: Test
testCaseLabel = TestCase $ assertBool 
                "Casing over label failed to typecheck"
                (typecheckSourceString
                    " case `A 5 of                  \
                    \   { `A x -> x }               ")

testCaseLabelMismatch :: Test
testCaseLabelMismatch = TestCase $ assertBool
                        "Casing over mismatched labels typechecked"      
                         (not $ typecheckSourceString
                                    "case `A 4 of                  \
                                    \    { `B x -> x }               ")

testCaseReturnTypeMismatch :: Test
testCaseReturnTypeMismatch = TestCase $ assertBool
                             "Case with return type mismatch failed to typecheck"
                             (typecheckSourceString
                                 "case x of {\
                                 \    int -> 0;\
                                 \    char -> 'a'}")

-- Tests that ensure function applications typecheck correctly
functionCases :: Test
functionCases = TestList [testFunctionDef, testIdentityApplication, testFuncApply, testFuncApplyVar, testFuncApplyMismatch, testSelfApplyApplication, testYCombinator, testYCombinatorAppl]

testFunctionDef :: Test
testFunctionDef = TestCase $ assertBool
                  "Function definition failed to typecheck"
                  (typecheckSourceString "(fun x -> x)")

testIdentityApplication :: Test
testIdentityApplication = TestCase $ assertBool
                          "Application of identity function to itself failed to typecheck"
                          (typecheckSourceString "(fun x -> x) (fun x -> x)")

testFuncApply :: Test
testFuncApply = TestCase $ assertBool
                "\"(fun x -> plus x 1) 1\" failed to typecheck"
                 (typecheckAst (Appl (Func (ident "x") (Plus (Var (ident "x")) (PrimInt 1))) (PrimInt 1))) 

testFuncApplyVar :: Test
testFuncApplyVar = TestCase $ assertBool
                   "\"(fun n -> plus n 2) x\" failed to typecheck"
                   (typecheckAst (Appl (Func (ident "n") (Plus (Var (ident "n")) (PrimInt 2))) (Var (ident "x"))))

testFuncApplyMismatch :: Test
testFuncApplyMismatch = TestCase $ assertBool
                        "\"(fun x -> plus x 1) \'a\'\" typechecked"
                         (not $ typecheckAst (Appl (Func (ident "x") (Plus (Var (ident "x")) (PrimInt 1))) (PrimChar 'a')))

testSelfApplyApplication :: Test
testSelfApplyApplication = TestCase $ assertBool
                           "Application of self application to itself failed to typecheck"
                           (typecheckSourceString "(fun x -> x x) (fun x -> x x)")

testYCombinator :: Test
testYCombinator = TestCase $ assertBool
                  "YCombinator failed to typecheck"
                  (typecheckSourceString "fun body -> (fun f -> fun arg -> f f arg) (fun this -> fun arg -> body (this this) arg)")
 

testYCombinatorAppl :: Test
testYCombinatorAppl = TestCase $ assertBool
                  "YCombinator application failed to typecheck"
                  (typecheckAst (Appl (Appl (Func (ident "body") (Appl (Func (ident "f") (Func (ident "arg") (Appl (Appl (Var (ident "f")) (Var (ident "f"))) (Var (ident "arg"))))) (Func (ident "this") (Func (ident "arg") (Appl (Appl (Var (ident "body")) (Appl (Var (ident "this")) (Var (ident "this")))) (Var (ident "arg"))))))) (Func (ident "this") (Func (ident "x") (Case (Equal (Var (ident "x")) (PrimInt 0)) [(ChiLabel (labelName "True") (ident "z"),PrimInt 0),(ChiLabel (labelName "False") (ident "z"),Plus (Var (ident "x")) (Appl (Var (ident "this")) (Minus (Var (ident "x")) (PrimInt 1))))])))) (PrimInt 5)))

-- Tests that ensure checks for equality typecheck correctly
equalityCases :: Test
equalityCases = TestList [testEqualInt, testEqualChar, testEqualIntChar, testEqualIntUnit, testEqualTrue, testEqualTrueFalse, testEqualFunction, testEqualLabel, testEqualLabelMismatch, testEqualUnit]

testEqualInt :: Test
testEqualInt = TestCase $ assertBool
               "Testing for equality on integers failed to typecheck"
               (typecheckAst (Equal (PrimInt 1) (PrimInt 1)))

testEqualChar :: Test
testEqualChar = TestCase $ assertBool
                "Testing for equality on characters failed to typecheck"
                (typecheckAst (Equal (PrimChar 'a') (PrimChar 'a')))

testEqualIntChar :: Test
testEqualIntChar = TestCase $ assertBool
                   "Testing for equality of integer and character typechecked"
                   (not $ typecheckAst (Equal (PrimInt 1) (PrimChar 'a')))

testEqualIntUnit :: Test
testEqualIntUnit = TestCase $ assertBool
                   "Testing for equality of integer and unit typechecked"
                   (not $ typecheckAst (Equal (PrimInt 1) PrimUnit))

testEqualTrue :: Test
testEqualTrue = TestCase $ assertBool
                         "Testing equality with true failed to typecheck"
                         (typecheckAst (Equal (Label (labelName "True") PrimUnit) (Label (labelName "True") PrimUnit)))

testEqualTrueFalse :: Test
testEqualTrueFalse = TestCase $ assertBool
                         "Testing equality with true and false failed to typecheck"
                         (not $ typecheckAst (Equal (Label (labelName "True") PrimUnit) (Label (labelName "False") PrimUnit)))

testEqualFunction :: Test
testEqualFunction = TestCase $ assertBool
                    "Testing equality of functions typedchecked"
                    (not $ typecheckAst (Equal (Func (ident "x") (Var (ident "x"))) (Func (ident "x") (Var (ident "x")))))

testEqualLabel :: Test
testEqualLabel = TestCase $ assertBool
                 "Testing equality of labels failed to typecheck"
                  (typecheckAst (Equal (Label (labelName "A") PrimUnit) (Label (labelName "A") PrimUnit)))

testEqualLabelMismatch :: Test
testEqualLabelMismatch = TestCase $ assertBool
                         "Testing equality for mismatched labels typechecked"
                         (not $ typecheckAst (Equal (Label (labelName "A") PrimUnit) (Label (labelName "B") PrimUnit)))

testEqualUnit :: Test
testEqualUnit = TestCase $ assertBool
                "Testing equality of unit typedchecked"
                (not $ typecheckAst (Equal PrimUnit PrimUnit))

-- Tests that ensure onions typecheck correctly
onionCases :: Test
onionCases = TestList [testOnionPlus, testOnionPlusNoInt, testOnionOnionPlus, testCompoundOnionPlus, testSubOnionPlus, testOnionFuncApply]

testOnionPlus :: Test
testOnionPlus = TestCase $ assertBool
                "Adding onion with integer component and integer failed to typecheck"
                (typecheckAst (Plus (Onion (PrimInt 2) (PrimChar 'b')) (PrimInt 2)))

testOnionPlusNoInt :: Test
testOnionPlusNoInt = TestCase $ assertBool
                     "Adding onion with no integer component and integer typechecked"
                     (typecheckAst (Plus (Onion (Label (labelName "True") PrimUnit) (PrimChar 'z')) (PrimInt 2)))

testOnionOnionPlus :: Test
testOnionOnionPlus = TestCase $ assertBool
                     "Adding two onions with integer components failed to typecheck"
                     (typecheckAst (Plus (Onion (PrimInt 2) (PrimChar 'x')) (Onion (PrimChar 'y') (PrimInt 2))))

testCompoundOnionPlus :: Test
testCompoundOnionPlus = TestCase $ assertBool
                        "Adding compound onions with integer components failed to typecheck"
                        (typecheckAst (Plus (Onion (PrimInt 2) (Onion (PrimChar 'a') PrimUnit)) (Onion (Onion (PrimInt 2) (PrimChar 'b')) PrimUnit)))

testSubOnionPlus :: Test
testSubOnionPlus = TestCase $ assertBool
                   "Adding onion and subonion with integers components failed to typecheck"
                   (typecheckAst (Plus (Onion (PrimInt 1) (Onion (PrimChar 'a') PrimUnit)) (Onion (PrimChar 'a') (Onion (PrimInt 1) PrimUnit))))

testOnionFuncApply :: Test
testOnionFuncApply = TestCase $ assertBool
                    "Application of function component within onion failed to typecheck"
                    (typecheckSourceString "(1 & (fun x -> x)) 1")
