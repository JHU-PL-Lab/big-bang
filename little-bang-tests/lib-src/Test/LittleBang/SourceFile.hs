module Test.LittleBang.SourceFile
( Test.LittleBang.SourceFile.generateTests
, LittleBangSourceFileTestConfig(..)
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

import Language.TinyBang.Ast as TBA
import Language.TinyBang.Interpreter
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Utils.Syntax.Location
import Language.TinyBang.TypeSystem
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
      , lbsftTypeSystem :: TypeSystem
      }

testsPath :: FilePath
testsPath = "tests"

generateTests :: LittleBangSourceFileTestConfig -> IO Test
generateTests (LittleBangSourceFileTestConfig
                { lbsftFilter = configFilter
                , lbsftTypeSystem = ts
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
    execute :: FilePath -> String -> Expectation -> EitherT String IO ()
    execute filename source expectation = do
      -- We should always be able to parse successfully and get a result from
      -- the typechecker
      let doc = NamedDocument filename
      tokens <- hoistEither $ lexLittleBang doc source
      ast <- hoistEither $ parseLittleBang doc tokens
      tbAst <- hoistEither $ desugarAll ast
      let tcResult = typecheck' tbAst
      case expectation of
        ExpectMatches pattern patSrc -> do
          -- We're going to build a catch-all scape that determines whether or
          -- not the evaluated ast matches the provided pattern.  We won't need
          -- to typecheck it because it's a catch-all.
          let ast' =
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
                where genVar = LB.Ident generated "___lbUnitTestValueVar"
          tbAst' <- hoistEither $ desugarAll ast'
          evalResult' <- liftIO $ runEitherT $ eval tbAst'
          case (tcResult, evalResult') of
            (Left err, _) ->
              left $ display $ text "Expected match on" <+> makeDoc patSrc <+>
                               text "but instead produced a type error:" <+>
                               makeDoc err
            (Right _, Left (err, _)) ->
              left $ display $ text "Expected match on" <+> makeDoc patSrc <+>
                               text "but evaluation failed!  " <+> makeDoc err
            (Right _, Right (env, var)) ->
              case deepOnion (varMap env) var of
                Left err -> left $ display $
                  text "FATAL: Failed to convert to deep onion for analysis:"
                  <+> makeDoc var <+> text "in" <+> makeDoc (varMap env) <+>
                  parens (text "due to:" <+> makeDoc err)
                Right onion ->
                  if Map.member (TBA.LabelName generated "MatchSuccess") $
                      deepLabels onion
                    then right ()
                    else
                      case Map.lookup (TBA.LabelName generated "MatchFailure") $
                            deepLabels onion of
                        Nothing -> error $ "LB unit test did not generate " ++
                                         "a `MatchSuccess or `MatchFailure " ++
                                         "on pattern match expectation!"
                        Just value ->
                          left $ display $
                            text "Expected pattern match did not succeed:" <>
                            line <> indent 2 (align $
                              text "Value: " <+> makeDoc value <+>
                              text "Pattern: " <+> text patSrc)
        ExpectTypeFail ->
          case tcResult of
            Left _ -> right ()
            Right db ->
              left $ "Expected type failure but typechecking produced a " ++
                     "valid database: " ++ display db
      where
        typecheck' :: TBA.Expr -> Either (Set TypecheckError) ConstraintSet
        typecheck' expr = do
          let result = typecheck ts expr
          unless (Set.null $ typeErrors result) $ Left $ typeErrors result
          return $ allConstraints result
        desugarAll :: LB.Expr -> Either String TBA.Expr
        desugarAll lbAst = do
          desugaredAst <- desugarLittleBang lbAst
          tbnAst <- convertToTBNExpr desugaredAst 
          return $ aTranslate tbnAst
