{-|
  This module loads the source file tests.
-}
module Test.TinyBang.SourceFile
( sourceFileTests
) where

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
import Test.TinyBang.ValueDsl

testsPath :: FilePath
testsPath = "tests"

sourceFileTests :: IO Test
sourceFileTests = do
  dirContents <- getDirectoryContents testsPath
  let paths = map ((testsPath ++ [pathSeparator]) ++) $
                filter (endsWith ".tb") dirContents
  mtests <- mapM makeTestFromPath paths
  return $ TestLabel "Source file tests" $
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
          BadDeepOnionPredicateParse src err ->
            return $ Left $ filepath ++ ": could not parse expectation " ++ src
              ++ ": " ++ err
        Right expectations -> case expectations of
          [] -> return $ Left $ filepath ++ ": no expectation found"
          _:_:_ -> return $ Left $ filepath ++ ": multiple expectations found"
          [expectation] -> return $ case lexTinyBang filepath source of
            Left err -> Left $ filepath ++ ": Lexer failure: " ++ err
            Right tokens ->
              let context = ParserContext
                    { contextDocument = UnknownDocument -- TODO: fix
                    , contextDocumentName = filepath
                    } in
              case parseTinyBang context tokens of
                Left err -> Left $ filepath ++ ": Parser failure: " ++ err
                Right ast -> Right $ createTest filepath expectation ast
    toExpectation :: String -> Either NoExpectation Expectation
    toExpectation str =
      case afterPart "# EXPECT:" str of
        Just src ->
          let src' = trim src in
          case parseValueDslPredicate src' of
            Left err -> Left $ BadDeepOnionPredicateParse src' err
            Right predicate -> Right $ Pass predicate src'
        Nothing -> Left NoExpectationFound
    trim :: String -> String
    trim str = reverse $ trimFront $ reverse $ trimFront str
      where
        trimFront :: String -> String
        trimFront = dropWhile (== ' ')
    beginsWith :: String -> String -> Bool
    beginsWith pfx str = pfx == take (length pfx) str
    endsWith :: String -> String -> Bool
    endsWith sfx str = sfx == drop (length str - length sfx) str
    afterPart :: String -> String -> Maybe String
    afterPart pfx str = if beginsWith pfx str then Just $ drop (length pfx) str
                                              else Nothing
    createTest :: FilePath -> Expectation -> Expr -> Test
    createTest filepath expectation expr =
      let result = eval expr in
      case expectation of
        Pass predicate predSrc -> TestLabel filepath $ TestCase $
          case result of
            Left (err,_) ->
              assertString $ "Expected " ++ display predSrc
                ++ " but error occurred: " ++ show err
            Right (env,var) ->
              let monion = deepOnion (flowVarMap env) (cellVarMap env) var in
              case monion of
                Left failure ->
                  error $ "Evaluator produced a result which did not " ++ 
                          "convert to an onion!  " ++ show failure
                Right onion ->
                  assertBool ("Expected " ++ display predSrc
                      ++ " but evaluation produced: " ++ display onion) $
                    predicate onion

data NoExpectation
  = NoExpectationFound
  | BadDeepOnionPredicateParse
      String -- ^ The source string for the predicate.
      String -- ^ The error message from the lexer/parser.
  deriving (Eq, Ord, Show)

data Expectation
  = Pass
      DeepOnionPredicate -- ^ The predicate that the result must match
      String -- ^ The original source of the predicate (for display purposes)
  -- TODO: add cases for expected failure
