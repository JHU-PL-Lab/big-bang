module Executables.Interpreter.SourceInterpreter
( stringyInterpretSource
, interpretSource
, InterpreterConfiguration(..)
) where

import Data.Map (Map)

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Parser

data InterpreterError
  = LexerFailure String
  | ParserFailure String
  -- TODO: type error stuff
  | EvaluationFailure EvalError [Clause]
  | EvaluationDisabled
  deriving (Eq, Ord, Show)

data InterpreterResult
  = InterpreterResult
      -- |The variable containing the result of computation.
      FlowVar
      -- |The mapping from flow variables to their values.
      (Map FlowVar Value)
      -- |The mapping from cell variables to their contents.
      (Map CellVar FlowVar)
  deriving (Eq, Ord, Show)
  
data InterpreterConfiguration
  = InterpreterConfiguration
    { typechecking :: Bool
    , evaluating :: Bool }
  
-- |Interprets the provided String as a TinyBang expression.  This value will be
--  lexed, parsed, typechecked, and executed.
interpretSource :: InterpreterConfiguration
                -> String -> Either InterpreterError InterpreterResult
interpretSource interpConf src = do
  tokens <- doStep LexerFailure $ lexTinyBang "<stdin>" src
  ast <- doStep ParserFailure $ parseTinyBang 
              ParserContext { contextDocument = UnknownDocument
                            , contextDocumentName = "<stdin>" }
              tokens
  if evaluating interpConf
    then do
      (env,var) <- doStep evalFail $ eval ast
      return $ InterpreterResult var (flowVarMap env) (cellVarMap env)
    else
      Left EvaluationDisabled
  where
    doStep errConstr computation =
      case computation of
        Left err -> Left $ errConstr err
        Right ans -> Right ans
    evalFail (err,cls) = EvaluationFailure err cls

-- |Interprets the provided String as a TinyBang expression.  This routine is
--  similar to @interpretSource@ from
--  @Executables.Interpreter.SourceInterpreter@ but converts the result to a
--  user-readable string.
stringyInterpretSource :: InterpreterConfiguration -> String -> String
stringyInterpretSource interpConf exprSrc =
  case interpretSource interpConf exprSrc of
    Left err -> case err of
      LexerFailure msg -> "Lexer error: " ++ msg
      ParserFailure msg -> "Parser error: " ++ msg
      -- TODO: typechecking case
      EvaluationFailure evalErr _ -> "Evaluation error: " ++ case evalErr of
        IllFormedExpression ill -> "Ill-formed expression: " ++ case ill of
          DuplicateFlowBinding x -> "Duplicate flow variable binding: "
                                    ++ unFlowVar x
          DuplicateFlowUse x -> "Duplicate flow variable use: "
                                ++ unFlowVar x
          DuplicateCellBinding y -> "Duplicate cell variable binding: "
                                    ++ unCellVar y
          InvalidExpressionEnd cl -> "Invalid ending clause for expression: "
                                     ++ show cl
          EmptyExpression -> "Empty subexpression"
        FlowVarNotClosed x -> "Flow variable not closed: " ++ unFlowVar x
        CellVarNotClosed y -> "Cell variable not closed: " ++ unCellVar y
        ProjectionFailure x proj -> "Could not project " ++ show proj
                                        ++ " from " ++ unFlowVar x
        ApplicationFailure x1 x2 -> "Could not apply " ++ unFlowVar x1
                                        ++ " to " ++ unFlowVar x2
      EvaluationDisabled -> "(evaluation disabled)"
    Right (InterpreterResult x fvs cvs) ->
      "*** Flow variables: \n" ++
      show fvs ++ "\n" ++
      "*** Cell variables: \n" ++
      show cvs ++ "\n" ++
      "*** Result variable: " ++
      unFlowVar x
