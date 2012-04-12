module Language.MicroBang.Test.UtilFunctions
( xEval
, xCont
, xLexs
, xPars
, fPars
, lexParseEval
, MicroBangCode
, Result
, labelName
, ident
, module Test.HUnit
, Token(..)
, makeState
)
where

import Prelude hiding (lex)

import Test.HUnit
import qualified Data.IntMap as IntMap

import Language.MicroBang.Interpreter.Interpreter
import Language.MicroBang.Interpreter.SourceInterpreter
import Language.MicroBang.Ast (vmPair, Evaluated)
import Language.MicroBang.Syntax.Lexer (Token)
import qualified Language.MicroBang.Ast as A (Expr, Value)
import qualified Language.MicroBang.Syntax.Lexer as L
  (lexMicroBang)
import Language.MicroBang.Syntax.Lexer (Token(..))
import Language.MicroBang.Types.UtilTypes (labelName, ident)
import Utils.Render.Display (display, Display)

type MicroBangCode = String
type Result = A.Value

xEval :: (Display v, Evaluated v, ?debug :: Bool) => MicroBangCode -> v -> Test
xEval code expectedResult =
  label ~: TestCase $ case wrappedResult of
    EvalResult _ sOrF -> case sOrF of
                           EvalSuccess result ->
                             assertEqual ""
                                         (vmPair expectedResult)
                                         (vmPair result)
                           EvalFailure err -> assertFailure $
                                                "EvalFailure: " ++ display err
    _ -> assertFailure $
         "Expected evaluation to succeed but instead got " ++
         display wrappedResult
  where wrappedResult = evalStringTop code
        label = show code ++
                " was expected to produce " ++ display expectedResult

assertSuccess :: Assertion
assertSuccess = return ()

xCont :: (?debug :: Bool) => MicroBangCode -> Test
xCont code =
  label ~: case result of
    Contradiction _ _ -> TestCase $ assertSuccess
    EvalResult _ sOrF  -> case sOrF of
      EvalSuccess _ -> TestCase $ assertFailure $
         "Expression evaluated to " ++
         display result ++
         ", which did not produce a contradiction"
      EvalFailure _ -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expression evaluated to " ++
         display result ++
         ", which did not produce a contradiction"
  where label = show code ++
                " was expected to produce a contradiction"
        result = evalStringTop code

xLexs :: MicroBangCode -> [Token] -> Test
xLexs code expected =
  label ~: TestCase $ case L.lexMicroBang code of
    Left err -> assertFailure $ "Lexing failed with: " ++ err
    Right tokens -> assertEqual "" expected tokens
  where label = "Lexing " ++ show code

xPars :: (?debug :: Bool) => MicroBangCode -> A.Expr -> Test
xPars code expected =
  label ~: TestCase $ case evalStringTop code of
    LexFailure err -> assertFailure $ "Lex failed: " ++ err
    ParseFailure err -> assertFailure $ "Parse failed: " ++ show err
    --TypecheckFailure expr _ _ -> eq expr
    Contradiction expr _ -> eq expr
    EvalResult expr _ -> eq expr
    DerivationFailure expr _ -> eq expr
  where label = "Parsing " ++ show code
        eq = assertEqual "" expected

fPars :: (?debug :: Bool) => MicroBangCode -> Test
fPars code =
  label ~: TestCase $ case evalStringTop code of
    LexFailure err -> assertFailure $ "Lex failed: " ++ err
    ParseFailure _ -> assertSuccess
    --TypecheckFailure expr _ _ -> failWith expr
    Contradiction expr _ -> failWith expr
    EvalResult expr _ -> failWith expr
    DerivationFailure expr _ -> failWith expr
  where label = "Parsing " ++ show code
        failWith expr = assertFailure $ "Parse succeeded with " ++ display expr

lexParseEval :: (?debug :: Bool)
             => MicroBangCode -> A.Expr -> A.Value -> Test
lexParseEval code expr val =
  TestList [
  --xLexs code lex
            xPars code expr
           , xEval code val
           ]

makeState :: [(Int, A.Value)] -> IntMap.IntMap A.Value
makeState = IntMap.fromList
