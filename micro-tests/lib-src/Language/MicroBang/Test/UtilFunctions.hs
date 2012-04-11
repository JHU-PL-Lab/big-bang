module Language.MicroBang.Test.UtilFunctions
( xEval
, xType
, xCont
--, xNotC
--, xDLbl
--, xDBnd
--, xErrT
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
--import qualified Language.MicroBang.Types.TypeInference as TI
--  (TypeInferenceError (..))
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

xType :: (?debug :: Bool) => MicroBangCode -> Test
xType code =
  label ~: TestCase $ case evalStringTop code of
    EvalResult _ _ -> assertSuccess
    result@_ -> assertFailure $
         "Expected closure to succeed but instead got " ++
         display result
  where label = show code ++
                " was expected to typecheck with no contradictions."

assertSuccess :: Assertion
assertSuccess = return ()

xCont :: (?debug :: Bool) => MicroBangCode -> Test
xCont code =
  label ~: case result of
    Contradiction _ _ -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expression evaluated to " ++
         display result ++
         ", which did not produce a contradiction"
  where label = show code ++
                " was expected to produce a contradiction"
        result = evalStringTop code

--xNotC :: (?debug :: Bool) => MicroBangCode -> Test
--xNotC code =
--  label ~: case result of
--    TypecheckFailure _ (TI.NotClosed _) _ -> TestCase $ assertSuccess
--    _ -> TestCase $ assertFailure $
--         "Expected NotClosed but instead got " ++
--         display result
--  where label = show code ++
--                " was expected to produce a contradiction"
--        result = evalStringTop code

--xErrT :: (?debug :: Bool)
--      => String -> (TI.TypeInferenceError -> Bool) -> MicroBangCode -> Test
--xErrT msg pred code =
--  label ~: case result of
--    TypecheckFailure _ tie _ | pred tie -> TestCase $ assertSuccess
--    _ -> TestCase $ assertFailure $
--         "Expected " ++ msg ++ " but instead got " ++ display result
--  where label = show code ++
--                " was expected to produce " ++ msg
--        result = evalStringTop code

--xDBnd :: (?debug :: Bool) => MicroBangCode -> Test
--xDBnd = xErrT "DoubleBound" $ \x ->
--          case x of
--            TI.DoubleBound{} -> True
--            _ -> False

--xDLbl :: (?debug :: Bool) => MicroBangCode -> Test
--xDLbl = xErrT "DoubleLabel" $ \x ->
--          case x of
--            TI.DoubleLabel{} -> True
--            _ -> False

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

-- label :: (Evaluated a) => String -> a -> Result
-- label name contents =
--   (A.VLabel lbl nextCell, IntMap.insert nextCell (value contents) state)
--   where lbl = labelName name
--         nextCell =
--           maybe 0 (inc . fst . fst) $ IntMap.maxViewWithKey state
--         inc = (+ 1)
--         state = mapping contents