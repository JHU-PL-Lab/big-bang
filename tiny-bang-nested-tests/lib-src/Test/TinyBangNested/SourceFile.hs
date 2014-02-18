module Test.TinyBangNested.SourceFile
( Test.TinyBangNested.SourceFile.generateTests
, TinyBangNestedSourceFileTestConfig(..)
) where

import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe
import Test.HUnit

import Language.TinyBang.Ast as TBA
import Language.TinyBang.Interpreter
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Syntax.Location
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.TypeInference
import Language.TinyBang.Utils.Display
import Language.TinyBangNested.Ast as TBN
import Language.TinyBangNested.ATranslator
import Language.TinyBangNested.Syntax.Lexer
import Language.TinyBangNested.Syntax.Parser
import Test.TinyBang.TestUtils.SourceFileTest
import Test.TinyBangNested.SourceFile.Expectation

{- |Defines the configuration of source file tests.
      * @sftFilter@ is an optional string that the name of the source file must
        match.
      * @sftDatabase@ is an empty database of the type which should be used.
-} 
data TinyBangNestedSourceFileTestConfig
  = TinyBangNestedSourceFileTestConfig
      { tbnsftFilter :: Maybe String
      , tbnsftDatabase :: SomeDisplayableConstraintDatabase
      }

testsPath :: FilePath
testsPath = "tests"

generateTests :: TinyBangNestedSourceFileTestConfig -> IO Test
generateTests (TinyBangNestedSourceFileTestConfig
                { tbnsftFilter = configFilter
                , tbnsftDatabase =
                    SomeDisplayableConstraintDatabase configDatabase
                }) =
  let sftConfig = SourceFileTestConfig
                    { sftDirectory = testsPath
                    , sftFilenameFilter =
                        case configFilter of
                          Nothing -> (".tbn" `isSuffixOf`)
                          Just name -> (== (name ++ ".tbn"))
                    , sftExpect = getExpectation
                    , sftExecute = execute
                    }
  in
  createSourceFileTests sftConfig
  where
    getExpectation :: FilePath -> String -> Either String Expectation
    getExpectation _ source =
      case mapM parseExpectation $ splitOn "\n" source of
        Left (expectationText, errMsg) ->
          Left $ "Failed to parse expectation \"" ++ expectationText ++
                 ": " ++ errMsg
        Right mexps ->
          case catMaybes mexps of
            [] -> Left "no expectation found"
            _:_:_ -> Left "multiple expectations found"
            [expectation] -> Right expectation
    execute :: FilePath -> String -> Expectation -> Either String ()
    execute filename source expectation = do
      -- We should always be able to parse successfully and get a result from
      -- the typechecker
      let doc = NamedDocument filename
      tokens <- lexTinyBangNested doc source
      ast <- parseTinyBangNested doc tokens
      let tbAst = aTranslate ast
      let tcResult = typecheck' configDatabase tbAst
      case expectation of
        ExpectMatches pattern patSrc ->
          -- We're going to build a catch-all scape that determines whether or
          -- not the evaluated ast matches the provided pattern.  We won't need
          -- to typecheck it because it's a catch-all.
          let ast' =
                let genVar = TBN.Var generated "___tbnUnitTestValueVar" in
                TBN.ExprAppl generated
                  (TBN.ExprOnion generated
                    (TBN.ExprScape generated pattern $
                      TBN.ExprLabelExp generated
                        (TBN.LabelName generated "MatchSuccess") $
                        TBN.ExprValEmptyOnion generated)
                    (TBN.ExprScape generated
                      (TBN.VariablePattern generated genVar) $
                      TBN.ExprLabelExp generated
                        (TBN.LabelName generated "MatchFailure") $
                        TBN.ExprVar generated genVar))
                  ast in
          let tbAst' = aTranslate ast' in
          let evalResult' = eval tbAst' in
          case (tcResult, evalResult') of
            (Left err, _) ->
              Left $ display $ text "Expected match on" <+> makeDoc patSrc <+>
                               text "but instead produced a type error:" <+>
                               makeDoc err
            (Right _, Left (err, _)) ->
              Left $ display $ text "Expected match on" <+> makeDoc patSrc <+>
                               text "but evaluation failed!  " <+> makeDoc err
            (Right _, Right (env, var)) ->
              case deepOnion (varMap env) var of
                Left err -> Left $ display $
                  text "FATAL: Failed to convert to deep onion for analysis:"
                  <+> makeDoc var <+> text "in" <+> makeDoc (varMap env) <+>
                  parens (text "due to:" <+> makeDoc err)
                Right onion ->
                  if Map.member (TBA.LabelName generated "MatchSuccess") $
                      deepLabels onion
                    then Right ()
                    else
                      case Map.lookup (TBA.LabelName generated "MatchFailure") $
                            deepLabels onion of
                        Nothing -> error $ "TBN unit test did not generate " ++
                                         "a `MatchSuccess or `MatchFailure " ++
                                         "on pattern match expectation!"
                        Just value ->
                          Left $ display $
                            text "Expected pattern match did not succeed:" <>
                            line <> indent 2 (align $
                              text "Value: " <+> makeDoc value <+>
                              text "Pattern: " <+> text patSrc)
        ExpectTypeFail ->
          case tcResult of
            Left _ -> Right ()
            Right db ->
              Left $ "Expected type failure but typechecking produced a " ++
                     "valid database: " ++ display db
      where
        typecheck' :: (ConstraintDatabase db, Display db)
                   => db -> TBA.Expr -> Either (TypecheckingError db) db
        typecheck' _ = typecheck
