module Language.TinyBang.Test.UtilFunctions
( xEval
, xType
, xCont
, xNotC
, xLexs
, xPars
, fPars
, lexParseEval
, TinyBangCode
, Result
, labelName
, ident
, module Test.HUnit
, Token(..)
, makeState
)
where

import Test.HUnit
import qualified Data.IntMap as IntMap

import Language.TinyBang.Interpreter.SourceInterpreter
import Language.TinyBang.Ast (vmPair, Evaluated)
import Language.TinyBang.Syntax.Lexer (Token)
import qualified Language.TinyBang.Ast as A (Expr, Value)
import qualified Language.TinyBang.Types.TypeInference as TI
  (TypeInferenceError (NotClosed))
import qualified Language.TinyBang.Syntax.Lexer as L
  (lexTinyBang)
import Language.TinyBang.Syntax.Lexer (Token(..))
import Language.TinyBang.Types.UtilTypes (labelName, ident)
import Language.TinyBang.Render.Display (display, Display)

type TinyBangCode = String
type Result = (A.Value, IntMap.IntMap A.Value)

xEval :: (Display v, Evaluated v) => TinyBangCode -> v -> Test
xEval code expectedResult =
  label ~: TestCase $ case wrappedResult of
    EvalResult _ sOrF -> case sOrF of
                           EvalSuccess result ->
                             assertEqual ""
                                         (canonicalize $ vmPair expectedResult)
                                         (canonicalize $ vmPair result)
                           EvalFailure err -> assertFailure $
                                                "EvalFailure: " ++ display err
    _ -> assertFailure $
         "Expected evaluation to succeed but instead got " ++
         display wrappedResult
  where wrappedResult = evalStringTop code
        label = show code ++
                " was expected to produce " ++ display expectedResult

xType :: TinyBangCode -> Test
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

xCont :: TinyBangCode -> Test
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

xNotC :: TinyBangCode -> Test
xNotC code =
  label ~: case result of
    TypecheckFailure _ (TI.NotClosed _) _ -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expected NotClosed but instead got " ++
         display result
  where label = show code ++
                " was expected to produce a contradiction"
        result = evalStringTop code

xLexs :: TinyBangCode -> [Token] -> Test
xLexs code expected =
  label ~: TestCase $ case L.lexTinyBang code of
    Left err -> assertFailure $ "Lexing failed with: " ++ err
    Right tokens -> assertEqual "" expected tokens
  where label = "Lexing " ++ show code

xPars :: TinyBangCode -> A.Expr -> Test
xPars code expected =
  label ~: TestCase $ case evalStringTop code of
    LexFailure err -> assertFailure $ "Lex failed: " ++ err
    ParseFailure err -> assertFailure $ "Parse failed: " ++ show err
    TypecheckFailure expr _ _ -> eq expr
    Contradiction expr _ -> eq expr
    EvalResult expr _ -> eq expr
  where label = "Parsing " ++ show code
        eq = assertEqual "" expected

fPars :: TinyBangCode -> Test
fPars code =
  label ~: TestCase $ case evalStringTop code of
    LexFailure err -> assertFailure $ "Lex failed: " ++ err
    ParseFailure _ -> assertSuccess
    TypecheckFailure expr _ _ -> failWith expr
    Contradiction expr _ -> failWith expr
    EvalResult expr _ -> failWith expr
  where label = "Parsing " ++ show code
        failWith expr = assertFailure $ "Parse succeeded with " ++ display expr

lexParseEval :: (Display a, Evaluated a)
             => TinyBangCode -> [Token] -> A.Expr -> a -> Test
lexParseEval code lex expr val =
  TestList [ xLexs code lex
           , xPars code expr
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
