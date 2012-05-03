{-# LANGUAGE  FlexibleContexts
            , FlexibleInstances
            , TypeSynonymInstances
            , ScopedTypeVariables
            #-}

module Language.LittleBang.Test.UtilFunctions
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
, LittleBangCode
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

import Language.LittleBang.Interpreter.SourceInterpreter
import Language.TinyBang.Ast (vmPair, Evaluated)
import Language.LittleBang.Syntax.Lexer (Token)
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Interpreter.Ast as IA
import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Types.TypeInference as TI
  (TypeInferenceError (..))
import qualified Language.LittleBang.Syntax.Lexer as L
  (lexLittleBang)
import qualified Language.LittleBang.Syntax.Parser as P
  (parseLittleBang)
import Language.LittleBang.Syntax.Lexer (Token(..))
import Language.TinyBang.Types.UtilTypes (labelName, ident)
import Utils.Render.Display (display, Display)
import qualified Language.TinyBang.Config as Cfg

type LittleBangCode = String
type Result = (TA.Value IA.Expr, IntMap.IntMap (TA.Value IA.Expr))

xEval :: (Display v, Evaluated IA.Expr v, ?conf :: Cfg.Config) => LittleBangCode -> v -> Test
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

xvEval :: (?conf :: Cfg.Config) => LittleBangCode -> TA.Value IA.Expr -> Test
xvEval = xEval

xsEval :: (?conf :: Cfg.Config) => LittleBangCode -> Result -> Test
xsEval = xEval

xType :: (?conf :: Cfg.Config) => LittleBangCode -> Test
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

xCont :: (?conf :: Cfg.Config) => LittleBangCode -> Test
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

xNotC :: (?conf :: Cfg.Config) => LittleBangCode -> Test
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
      => String -> (TI.TypeInferenceError -> Bool) -> LittleBangCode -> Test
xErrT msg pred code =
  label ~: case result of
    TypecheckFailure _ tie _ | pred tie -> TestCase $ assertSuccess
    _ -> TestCase $ assertFailure $
         "Expected " ++ msg ++ " but instead got " ++ display result
  where label = show code ++
                " was expected to produce " ++ msg
        result = evalStringTop code

xDBnd :: (?conf :: Cfg.Config) => LittleBangCode -> Test
xDBnd = xErrT "DoubleBound" $ \x ->
          case x of
            TI.DoubleBound{} -> True
            _ -> False

xDLbl :: (?conf :: Cfg.Config) => LittleBangCode -> Test
xDLbl = xErrT "DoubleLabel" $ \x ->
          case x of
            TI.DoubleLabel{} -> True
            _ -> False

xLexs :: LittleBangCode -> [Token] -> Test
xLexs code expected =
  label ~: TestCase $ case L.lexLittleBang code of
    Left err -> assertFailure $ "Lexing failed with: " ++ err
    Right tokens -> assertEqual "" expected tokens
  where label = "Lexing " ++ show code

xPars :: (?conf :: Cfg.Config) => LittleBangCode -> LA.Expr -> Test
xPars code expected =
  label ~: TestCase $
    case L.lexLittleBang code of
      Left err -> assertFailure $ "Lexing failed with: " ++ err
      Right tokens ->
        case P.parseLittleBang tokens of
          Left err -> assertFailure $ "Parsing failed with: " ++ display err
          Right ast -> assertEqual "" expected ast
  where label = "Parsing " ++ show code

fPars :: (?conf :: Cfg.Config) => LittleBangCode -> Test
fPars code =
  label ~: TestCase $
    case L.lexLittleBang code of
      Left err -> assertFailure $ "Lexing failed with: " ++ err
      Right tokens ->
        case P.parseLittleBang tokens of
          Left err -> assertSuccess
          Right ast -> assertFailure $ "Parsing succeeded with: " ++ display ast
  where label = "Parsing (expecting error) " ++ show code

lexParseEval :: (Display a, Evaluated IA.Expr a, ?conf :: Cfg.Config)
             => LittleBangCode -> [Token] -> LA.Expr -> a -> Test
lexParseEval code lex expr val =
  TestList [ xLexs code lex
           , xPars code expr
           , xEval code val
           ]

makeState :: [(Int, TA.Value IA.Expr)] -> IntMap.IntMap (TA.Value IA.Expr)
makeState = IntMap.fromList

-- label :: (Evaluated a) => String -> a -> Result
-- label name contents =
--   (A.VLabel lbl nextCell, IntMap.insert nextCell (value contents) state)
--   where lbl = labelName name
--         nextCell =
--           maybe 0 (inc . fst . fst) $ IntMap.maxViewWithKey state
--         inc = (+ 1)
--         state = mapping contents
