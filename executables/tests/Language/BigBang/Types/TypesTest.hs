module Language.BigBang.Types.TypesTest
( tests
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit hiding (Label)

import Language.BigBang.Ast
import qualified Language.BigBang.Types.Types as T
import qualified Language.BigBang.Types.TypeInference as I
import qualified Language.BigBang.Types.Closure as C
import Language.BigBang.Types.UtilTypes
import Language.BigBang.Interpreter.Interpreter
import Language.BigBang.Syntax.Parser
import Language.BigBang.Syntax.Lexer

tests :: Test
tests = TestList [primitiveBuiltinCases, caseCases, functionCases, equalityCases]

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
    not $ Set.member T.Bottom $
        C.calculateClosure $ snd $ (\x -> I.runTIM x Map.empty 0) $
            I.inferType expr


-- Test cases that ensure that built-in primitive operations typecheck on their
-- appropriate values

primitiveBuiltinCases :: Test
primitiveBuiltinCases = TestList [testPlusInt, testMinusInt, testCompoundInt, testPlusIntBad, testPlusIntChar, testPlusChar]

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
                  "Compound statement involving integers failed to typeCheck"  
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

testPlusChar :: Test
testPlusChar = TestCase $ assertBool
               "Addition of characters typechecked"
               (not $ typecheckAst (Plus (PrimChar 'a') (PrimChar 'a')))

testPlusIntBad :: Test
testPlusIntBad = TestCase $ assertBool
                 "Addition of units typechecked"
                 (not $ typecheckAst (Plus PrimUnit PrimUnit))

-- Test cases that ensure that case expressions resolve types correctly
caseCases :: Test
caseCases = TestList [testCaseLabel, testCaseLabelMismatch]

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
                                    " case `A 4 of                  \
                                    \   { `B x -> x }               ")

-- Tests that ensure function applications typecheck correctly
functionCases :: Test
functionCases = TestList [testFunctionDef, testFuncApply, testFuncApplyMismatch, testSelfApplyApplication, testYCombinator, testYCombinatorAppl]

testFunctionDef :: Test
testFunctionDef = TestCase $ assertBool
                  "Function definition failed to typecheck"
                  (typecheckSourceString "(fun x -> x)")

testFuncApply :: Test
testFuncApply = TestCase $ assertBool
                "\"(fun x -> plus x 1) 1\" failed to typecheck"
                 (typecheckAst (Appl (Func (ident "x") (Plus (Var (ident "x")) (PrimInt 1))) (PrimInt 1))) 

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
                  (typecheckSourceString "(\\f -> (\\x -> f (x x))) (\\x -> f (x x))")

testYCombinatorAppl :: Test
testYCombinatorAppl = TestCase $ assertBool
                  "YCombinator application failed to typecheck"
                  (typecheckAst (Appl 
                      (Appl 
                          (Func 
                              (ident "f") 
                              (Func 
                                  (ident "x") 
                                  (Appl 
                                      (Var (ident "f")) 
                                          (Appl 
                                              (Var (ident "x")) 
                                              (Var (ident "x"))))))
                      (Func 
                          (ident "x") 
                          (Appl 
                              (Var (ident "f")) 
                              (Appl 
                              (Var (ident "x")) 
                              (Var (ident "x"))))))
                      (Func 
                          (ident "x")
                              (Case 
                                  (Equal (Var (ident "x")) (PrimInt 0))
                                  [(ChiLabel (labelName "True") (ident "a"), PrimInt 0), (ChiLabel (labelName "False") (ident "a"), (Plus (Var (ident "x")) (Appl (Var (ident "f")) (Minus (Var (ident "x")) (PrimInt 1)))))]))))

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
