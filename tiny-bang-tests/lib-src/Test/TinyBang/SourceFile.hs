{-|
  This Test.TinyBang.SourceFilefile tests.
-}
module Test.TinyBang.SourceFile
( generateTests
, TinyBangSourceFileTestConfig(..)
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display
import Language.TinyBang.Interpreter
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.TypeSystem
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Syntax.Parser
import Language.TinyBang.Utils.Syntax.Location
import Test.TinyBang.TestUtils.ExpectDsl
import Test.TinyBang.TestUtils.SourceFileTest

{- |Defines the configuration of source file tests.
      * @sftFilter@ is an optional string that the name of the source file must
        match.
      * @sftDatabase@ is an empty database of the type which should be used.
-} 
data TinyBangSourceFileTestConfig
  = TinyBangSourceFileTestConfig
      { tbsftFilter :: Maybe String
      , tbsftTypeSystem :: TypeSystem
      }
      
testsPath :: FilePath
testsPath = "tests"

generateTests :: TinyBangSourceFileTestConfig -> IO Test
generateTests (TinyBangSourceFileTestConfig
                { tbsftFilter = configFilter
                , tbsftTypeSystem = ts
                }) =
  let sftConfig = SourceFileTestConfig
                    { sftDirectory = testsPath
                    , sftFilenameFilter =
                        case configFilter of
                          Nothing -> (".tb" `isSuffixOf`)
                          Just name -> (== (name ++ ".tb"))
                    , sftExpect = getExpectation
                    , sftExecute = execute
                    }
  in
  createSourceFileTests sftConfig
  where
    getExpectation :: FilePath -> String -> Either String Expectation
    getExpectation _ source =
        case mapM toExpectation $ splitOn "\n" source of
          Left (expectationText, errMsg) ->
            Left $ "Failed to parse expectation \"" ++ expectationText ++
                   ": " ++ errMsg
          Right mexps ->
            case catMaybes mexps of
              [] -> Left "no expectation found"
              _:_:_ -> Left "multiple expectations found"
              [expectation] -> Right expectation
      where
        -- |Parses an expectation from a source line.  The result is a
        --  @Right Nothing@ if the line has no expectation; it is a @Left@ if
        --  there is an expectation but it did not parse successfully.  The
        --  error reported includes the original string as well as the parser
        --  message.
        toExpectation :: String
                      -> Either (String, String) (Maybe Expectation)
        toExpectation lin =
          let pfx = "# EXPECT:" in
          let lin' = strip lin in
          if pfx `isPrefixOf` strip lin'
            then
              let expectText = drop (length pfx) lin' in
              case parseExpectDslPredicate expectText of
                Left err -> Left (expectText, err)
                Right expectation -> Right $ Just expectation
            else
              Right Nothing
    execute :: FilePath -> String -> Expectation -> EitherT String IO ()
    execute filename source expectation = do
      -- We should always be able to parse successfully and get a result from
      -- the typechecker
      let doc = NamedDocument filename
      tokens <- hoistEither $ lexTinyBang doc source
      ast <- hoistEither $ parseTinyBang doc tokens
      let tcResult = typecheck' ast
      case expectation of
        Pass predicate predSrc -> do
          res <- liftIO $ runEitherT $ eval ast
          case (tcResult, res) of
            (Left err, _) ->
              left $ "Expected " ++ display predSrc ++
                     " but type error occurred: " ++ display err
            (_, Left (err,_)) ->
              left $ "Expected " ++ display predSrc ++
                     " but error occurred: " ++ display err
            (_, Right (env,var)) ->
              let monion = deepOnion (varMap env) var in
              case monion of
                Left err ->
                  error $ "Evaluator produced a result which did not " ++ 
                          "convert to an onion!  " ++ display err
                Right onion ->
                  if predicate onion then right () else
                    left $ "Expected " ++ display predSrc ++
                           " but evaluation produced: " ++ display onion
        TypeFailure ->
            case tcResult of
              Left _ -> right ()
              Right db ->
                left $ "Expected type failure but typechecking produced a " ++
                       "valid database: " ++ display db
      where
        typecheck' :: Expr -> Either (Set TypecheckError) ConstraintSet
        typecheck' expr =
          let result = typecheck ts expr in
          if Set.null $ typeErrors result
            then return $ allConstraints result
            else Left $ typeErrors result
    strip :: String -> String
    strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
