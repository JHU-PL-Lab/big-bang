{-# LANGUAGE  FlexibleContexts
            , FlexibleInstances
            , TypeSynonymInstances
            , ScopedTypeVariables
            #-}

module Language.TinyBang.Test.UtilFunctions
( xEval
, xvEval
, xsEval
, xType
, xCont
, xNotC
, xDLbl
, xDBnd
, xErrT
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

import Prelude hiding (lex)

import Test.HUnit
import qualified Data.IntMap as IntMap

import Language.TinyBang.Interpreter.SourceInterpreter
import Language.TinyBang.Ast (vmPair, Evaluated)
import Language.TinyBang.Syntax.Lexer (Token, SourceLocation, defaultSourceLocation)
import qualified Language.TinyBang.Ast as A (Expr, Value)
import qualified Language.TinyBang.Interpreter.Ast as IA
import qualified Language.TinyBang.Types.TypeInference as TI
  (TypeInferenceError (..))
import qualified Language.TinyBang.Syntax.Lexer as L
  (lexTinyBang)
import Language.TinyBang.Syntax.Lexer (Token(..))
import Language.TinyBang.Types.UtilTypes (labelName, ident)
import Utils.Render.Display (display, Display)
import qualified Language.TinyBang.Config as Cfg

type TinyBangCode = String
type Result = (A.Value IA.Expr, IntMap.IntMap (A.Value IA.Expr))

xEval :: (Display v, Evaluated IA.Expr v, ?conf :: Cfg.Config) => TinyBangCode -> v -> Test
xEval code expectedResult =
  label ~: TestCase $ case wrappedResult of
    EvalResult _ sOrF -> case sOrF of
                           EvalSuccess result ->
                             assertEqual ""
                                         (canonicalize $ vmPair expectedResult)
                                         (canonicalize $ (vmPair result :: Result))
                           EvalFailure err -> assertFailure $
                                                "EvalFailure: " ++ display err
    _ -> assertFailure $
         "Expected evaluation to succeed but instead got " ++
         display wrappedResult
  where wrappedResult = evalStringTop code
        label = show code ++
                " was expected to produce " ++ display expectedResult

xvEval :: (?conf :: Cfg.Config) => TinyBangCode -> A.Value IA.Expr -> Test
xvEval = xEval

xsEval :: (?conf :: Cfg.Config) => TinyBangCode -> Result -> Test
xsEval = xEval

xType :: (?conf :: Cfg.Config) => TinyBangCode -> Test
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

xCont :: (?conf :: Cfg.Config) => TinyBangCode -> Test
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

xNotC :: (?conf :: Cfg.Config) => TinyBangCode -> Test
xNotC code =
  label ~: case result of
    TypecheckFailure _ (TI.NotClosed _) _ -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expected NotClosed but instead got " ++
         display result
  where label = show code ++
                " was expected to produce a contradiction"
        result = evalStringTop code

xErrT :: (?conf :: Cfg.Config)
      => String -> (TI.TypeInferenceError -> Bool) -> TinyBangCode -> Test
xErrT msg pred code =
  label ~: case result of
    TypecheckFailure _ tie _ | pred tie -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expected " ++ msg ++ " but instead got " ++ display result
  where label = show code ++
                " was expected to produce " ++ msg
        result = evalStringTop code

xDBnd :: (?conf :: Cfg.Config) => TinyBangCode -> Test
xDBnd = xErrT "DoubleBound" $ \x ->
          case x of
            TI.DoubleBound{} -> True
            _ -> False

xDLbl :: (?conf :: Cfg.Config) => TinyBangCode -> Test
xDLbl = xErrT "DoubleLabel" $ \x ->
          case x of
            TI.DoubleLabel{} -> True
            _ -> False

xLexs :: TinyBangCode -> [SourceLocation -> Token] -> Test
xLexs code expected =
  label ~: TestCase $ case L.lexTinyBang code of
    Left err -> assertFailure $ "Lexing failed with: " ++ err
    Right tokens -> assertEqual "" (map ($defaultSourceLocation) expected) tokens
  where label = "Lexing " ++ show code

xPars :: (?conf :: Cfg.Config) => TinyBangCode -> A.Expr -> Test
xPars code expected =
  label ~: TestCase $ case evalStringTop code of
    LexFailure err -> assertFailure $ "Lex failed: " ++ err
    ParseFailure err -> assertFailure $ "Parse failed: " ++ show err
    TypecheckFailure expr _ _ -> eq expr
    Contradiction expr _ -> eq expr
    EvalResult expr _ -> eq expr
  where label = "Parsing " ++ show code
        eq = assertEqual "" expected

fPars :: (?conf :: Cfg.Config) => TinyBangCode -> Test
fPars code =
  label ~: TestCase $ case evalStringTop code of
    LexFailure err -> assertFailure $ "Lex failed: " ++ err
    ParseFailure _ -> assertSuccess
    TypecheckFailure expr _ _ -> failWith expr
    Contradiction expr _ -> failWith expr
    EvalResult expr _ -> failWith expr
  where label = "Parsing " ++ show code
        failWith expr = assertFailure $ "Parse succeeded with " ++ display expr

lexParseEval :: (Display a, Evaluated IA.Expr a, ?conf :: Cfg.Config)
             => TinyBangCode -> [SourceLocation -> Token] -> A.Expr -> a
             -> Test
lexParseEval code lex expr val =
  TestList [ xLexs code lex
           , xPars code expr
           , xEval code val
           ]

makeState :: [(Int, A.Value IA.Expr)] -> IntMap.IntMap (A.Value IA.Expr)
makeState = IntMap.fromList

-- label :: (Evaluated a) => String -> a -> Result
-- label name contents =
--   (A.VLabel lbl nextCell, IntMap.insert nextCell (value contents) state)
--   where lbl = labelName name
--         nextCell =
--           maybe 0 (inc . fst . fst) $ IntMap.maxViewWithKey state
--         inc = (+ 1)
--         state = mapping contents
