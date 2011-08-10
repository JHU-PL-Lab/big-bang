module Language.BigBang.Types.TypesTest
( tests
) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
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
tests = TestList [primitiveBuiltinCases, caseCases]

-- A utility function which generates a typechecking result for source code.
-- The provided string is lexed, parsed, and typechecked as a Big Bang program.
-- This function produces True for a successful typecheck and False for
-- failure.
typecheckSourceString :: String -> Bool
typecheckSourceString src = typecheckAst $ parseBigBang $ lexBigBang src

typecheckAst :: Expr -> Bool
typecheckAst expr =
    not $ Set.member T.Bottom $
        C.calculateClosure $ snd $ (\x -> I.runTIM x Map.empty 0) $
            I.inferType expr

expectSuccess :: Bool -> Test
expectSuccess = (True ~=?)

expectFailure :: Bool -> Test
expectFailure = (False ~=?)

-- Test cases that ensure that built-in primitive operations typecheck on their
-- appropriate values
primitiveBuiltinCases :: Test
primitiveBuiltinCases = TestList [testPlusInt, testMinusInt, testCompoundInt, testPlusIntBad]

testPlusInt :: Test
testPlusInt = expectSuccess $ typecheckAst $ Plus (PrimInt 1) (PrimInt 2)

testMinusInt :: Test
testMinusInt = expectSuccess $ typecheckAst $ Minus (PrimInt 1) (PrimInt 2)

testCompoundInt :: Test
testCompoundInt = expectSuccess $ typecheckAst $
        Plus
            (Minus
                (Plus
                    (PrimInt 1)
                    (PrimInt 2))
                (PrimInt 3))
            (Plus
                (PrimInt (-2))
                (Minus
                    (PrimInt 4)
                    (PrimInt 0)))

testPlusIntBad :: Test
testPlusIntBad = expectFailure $ typecheckAst $ Plus PrimUnit PrimUnit

-- Test cases that ensure that case expressions resolve types correctly
caseCases :: Test
caseCases = TestList [testCaseLabel, testCaseLabelMismatch]

testCaseLabel :: Test
testCaseLabel = expectSuccess $ typecheckSourceString
        " case `A 5 of                  \
        \   { `A x -> x }               "

testCaseLabelMismatch :: Test
testCaseLabelMismatch = expectFailure $ typecheckSourceString
        " case `A 4 of                  \
        \   { `B x -> x }               "
