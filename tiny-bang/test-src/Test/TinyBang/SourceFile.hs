{-|
  This module loads the source file tests.
-}
module Test.TinyBang.SourceFile
( sourceFileTests
) where

import Data.List
import Data.List.Split (splitOn)
import System.Directory
import System.FilePath
import Test.HUnit

import Language.TinyBang.Ast
import Language.TinyBang.Display
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

sourceFileTests :: IO Test
sourceFileTests = do
  dirContents <- getDirectoryContents testsPath
  let paths = map ((testsPath ++ [pathSeparator]) ++) $
                filter (isSuffixOf ".tb") dirContents
  mtests <- mapM makeTestFromPath paths
  return $
    case sequence mtests of
      Left err -> TestCase $ assertString $ "Test construction failure: " ++ err 
      Right tests -> TestList tests
  where
    makeTestFromPath :: FilePath -> IO (Either String Test)
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
            return $ Left $ filepath ++ ": could not parse expectation " ++ src
              ++ ": " ++ err
        Right expectations ->
          let doc = NamedDocument filepath in
          case expectations of
            [] -> return $ Left $ filepath ++ ": no expectation found"
            _:_:_ -> return $ Left $ filepath ++ ": multiple expectations found"
            [expectation] -> return $ case lexTinyBang doc source of
              Left err -> Left $ filepath ++ ": Lexer failure: " ++ err
              Right tokens ->
                case parseTinyBang doc tokens of
                  Left err -> Left $ filepath ++ ": Parser failure: " ++ err
                  Right ast -> Right $ createTest filepath expectation ast
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
      {-
      let tcResult = typecheck expr
          tcResult :: Either (TypecheckingError SimpleConstraintDatabase)
                        SimpleConstraintDatabase
      in
      -}
      let tcResult = Right () :: Either () () in -- TODO: replace
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
                Left failure ->
                  error $ "Evaluator produced a result which did not " ++ 
                          "convert to an onion!  " ++ display failure
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

