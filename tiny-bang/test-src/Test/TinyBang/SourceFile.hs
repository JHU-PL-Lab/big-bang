{-|
  This module loads the source file tests.
-}
module Test.TinyBang.SourceFile
( tests
, filteredTests
) where

import Data.List
import Data.List.Split (splitOn)
import System.Directory
import System.FilePath
import Test.HUnit

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display
import Language.TinyBang.Interpreter
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Parser
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple
import Language.TinyBang.TypeSystem.TypeInference
import Test.TinyBang.ExpectDsl

testsPath :: FilePath
testsPath = "tests"

tests :: IO Test
tests = generateTests Nothing

filteredTests :: String -> IO Test
filteredTests = generateTests . Just

generateTests :: Maybe String -> IO Test
generateTests filterName = do
  dirContents <- getDirectoryContents testsPath
  let paths = map ((testsPath ++ [pathSeparator]) ++) $ 
                filter dirFilter dirContents
  mtests <- mapM makeTestFromPath paths
  return $ TestList mtests
  where
    dirFilter :: String -> Bool
    dirFilter pathname =
      case filterName of
        Nothing -> ".tb" `isSuffixOf` pathname
        Just str -> pathname == str ++ ".tb"
    failure :: String -> Test
    failure = TestCase . assertFailure
    makeTestFromPath :: FilePath -> IO Test
    makeTestFromPath filepath = do
      source <- readFile filepath
      -- get the possible expectations
      let mexpectations = map toExpectation $ splitOn "\n" source
      -- filter out acceptable errors
      let mexpectations' = sequence $
            filter (\x->case x of {Left NoExpectationFound -> False; _ -> True})
              mexpectations
      -- if there are still errors, report them
      case mexpectations' of
        Left noExpectation -> case noExpectation of
          NoExpectationFound ->
            error "Test.TinyBang.SourceFile: didn't we just filter this out?"
          BadExpectationParse src err ->
            return $ failure $
              filepath ++ ": could not parse expectation " ++ src ++ ": " ++ err
        Right expectations ->
          let doc = NamedDocument filepath in
          case expectations of
            [] -> return $ failure $ filepath ++ ": no expectation found"
            _:_:_ -> return $ failure $
                      filepath ++ ": multiple expectations found"
            [expectation] -> return $ case lexTinyBang doc source of
              Left err -> failure $ filepath ++ ": Lexer failure: " ++ err
              Right tokens ->
                case parseTinyBang doc tokens of
                  Left err -> failure $ filepath ++ ": Parser failure: " ++ err
                  Right ast -> createTest filepath expectation ast
    toExpectation :: String -> Either NoExpectation Expectation
    toExpectation str =
      case afterPart "# EXPECT:" str of
        Just src ->
          let src' = trim src in
          case parseExpectDslPredicate src' of
            Left err -> Left $ BadExpectationParse src' err
            Right expectation -> Right expectation
        Nothing -> Left NoExpectationFound
    trim :: String -> String
    trim str = reverse $ trimFront $ reverse $ trimFront str
      where
        trimFront :: String -> String
        trimFront = dropWhile (== ' ')
    afterPart :: String -> String -> Maybe String
    afterPart pfx str = if pfx `isPrefixOf` str
                          then Just $ drop (length pfx) str
                          else Nothing
    createTest :: FilePath -> Expectation -> Expr -> Test
    createTest filepath expectation expr =
      let tcResult = typecheck expr
          tcResult :: Either (TypecheckingError SimpleConstraintDatabase)
                        SimpleConstraintDatabase
      in
      let result = eval expr in
      case expectation of
        Pass predicate predSrc -> TestLabel filepath $ TestCase $
          case (tcResult, result) of
            (Left err, _) ->
              assertString $ "Expected " ++ display predSrc
                ++ " but type error occurred: " ++ display err
            (_, Left (err,_)) ->
              assertString $ "Expected " ++ display predSrc
                ++ " but error occurred: " ++ display err
            (_, Right (env,var)) ->
              let monion = deepOnion (varMap env) var in
              case monion of
                Left err ->
                  error $ "Evaluator produced a result which did not " ++ 
                          "convert to an onion!  " ++ display err
                Right onion ->
                  assertBool ("Expected " ++ display predSrc
                      ++ " but evaluation produced: " ++ display onion) $
                    predicate onion
        TypeFailure -> TestLabel filepath $ TestCase $
          case tcResult of
            Left _ -> assert True
            Right db ->
              assertString $ "Expected type failure but typechecking produced "
                          ++ "a valid database: " ++ display db

data NoExpectation
  = NoExpectationFound
  | BadExpectationParse
      String -- ^ The source string for the predicate.
      String -- ^ The error message from the lexer/parser.
  deriving (Eq, Ord, Show)

