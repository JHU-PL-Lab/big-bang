module Test.LittleBang.SourceFile
( Test.LittleBang.SourceFile.generateTests
, LittleBangSourceFileTestConfig(..)
) where

import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe
import Test.HUnit

import Language.TinyBang.Ast as TBA
import Language.TinyBang.Interpreter
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Utils.Syntax.Location
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.TypeInference
import Language.TinyBang.Utils.Display
import Language.TinyBangNested.ATranslator
import Language.LittleBang.Ast as LB
import Language.LittleBang.TBNConversion
import Language.LittleBang.Translator
import Language.LittleBang.Syntax.Lexer
import Language.LittleBang.Syntax.Parser
import Test.TinyBang.TestUtils.SourceFileTest
import Test.LittleBang.SourceFile.Expectation

{- |Defines the configuration of source file tests.
      * @sftFilter@ is an optional string that the name of the source file must
        match.
      * @sftDatabase@ is an empty database of the type which should be used.
-} 
data LittleBangSourceFileTestConfig
  = LittleBangSourceFileTestConfig
      { lbsftFilter :: Maybe String
      , lbsftDatabase :: SomeDisplayableConstraintDatabase
      }

testsPath :: FilePath
testsPath = "tests"

generateTests :: LittleBangSourceFileTestConfig -> IO Test
generateTests (LittleBangSourceFileTestConfig
                { lbsftFilter = configFilter
                , lbsftDatabase =
                    SomeDisplayableConstraintDatabase configDatabase
                }) =
  let sftConfig = SourceFileTestConfig
                    { sftDirectory = testsPath
                    , sftFilenameFilter =
                        case configFilter of
                          Nothing -> (".lb" `isSuffixOf`)
                          Just name -> (== (name ++ ".lb"))
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
                 "\": " ++ errMsg
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
      tokens <- lexLittleBang doc source
      ast <- parseLittleBang doc tokens
      tbAst <- desugarAll ast
      let tcResult = typecheck' configDatabase tbAst
      case expectation of
        ExpectMatches pattern patSrc -> do
          -- We're going to build a catch-all scape that determines whether or
          -- not the evaluated ast matches the provided pattern.  We won't need
          -- to typecheck it because it's a catch-all.
          let ast' =
                let genVar = LB.Var generated "___lbUnitTestValueVar" in
                LB.TExprAppl generated
                  (LB.TExprOnion generated
                    (LB.TExprScape generated pattern $
                      LB.TExprLabelExp generated
                        (LB.LabelName generated "MatchSuccess") $
                        LB.TExprValEmptyOnion generated)
                    (LB.TExprScape generated
                      (LB.VariablePattern generated genVar) $
                      LB.TExprLabelExp generated
                        (LB.LabelName generated "MatchFailure") $
                        LB.TExprVar generated genVar))
                  ast
          tbAst' <- desugarAll ast'
          let evalResult' = eval tbAst'
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
                        Nothing -> error $ "LB unit test did not generate " ++
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
        desugarAll :: LB.Expr -> Either String TBA.Expr
        desugarAll lbAst = do
          desugaredAst <- desugarLittleBang lbAst
          tbnAst <- convertToTBNExpr desugaredAst 
          return $ aTranslate tbnAst
