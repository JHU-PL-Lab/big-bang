{-# LANGUAGE GADTs, ExistentialQuantification, LambdaCase #-}

{-|
  This module loads the source file tests.
-}
module Test.TinyBang.SourceFile
( tests
, generateTests
, SourceFileTestConfig(..)
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
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.TypeInference
import Test.TestUtils.ExpectDsl

testsPath :: FilePath
testsPath = "tests"

{- |Defines the configuration of source file tests.
      * @sftFilter@ is an optional string that the name of the source file must
        match.
      * @sftDatabase@ is an empty database of the type which should be used.
-} 
data SourceFileTestConfig
  = SourceFileTestConfig
      { sftFilter :: Maybe String
      , sftDatabase :: SomeDisplayableConstraintDatabase
      }
      
defaultSourceFileTestConfig :: SourceFileTestConfig
defaultSourceFileTestConfig =
  SourceFileTestConfig
    { sftFilter = Nothing
    , sftDatabase = SomeDisplayableConstraintDatabase $
                      (CDb.empty :: IndexedConstraintDatabase)
    }

tests :: IO Test
tests = generateTests defaultSourceFileTestConfig

-- |Generates source file tests.  
generateTests :: SourceFileTestConfig -> IO Test
-- Unpacking the config in the arguments to get the existentials
generateTests
    (SourceFileTestConfig
      { sftFilter = configFilter
      , sftDatabase = SomeDisplayableConstraintDatabase configDatabase
      }) =
  do
    dirContents <- getDirectoryContents testsPath
    let paths = map ((testsPath ++ [pathSeparator]) ++) $ 
                  filter dirFilter dirContents
    mtests <- mapM makeTestFromPath paths
    return $ TestList mtests
    where
      dirFilter :: String -> Bool
      dirFilter pathname =
        case configFilter of
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
              filter (\case {Left NoExpectationFound -> False; _ -> True})
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
      -- |A typechecking routine whose sole purpose is to allow input-driven
      --  control of the output type.
      typecheck' :: (ConstraintDatabase db, Display db)
                 => db -> Expr -> Either (TypecheckingError db) db
      typecheck' _ = typecheck
      createTest :: FilePath -> Expectation -> Expr -> Test
      createTest filepath expectation expr =
        let tcResult = typecheck' configDatabase expr in
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

