module Executables.Interpreter.SourceInterpreter
( stringyInterpretSource
, interpretSource
, InterpreterConfiguration(..)
) where

import Data.Map (Map)

import Language.TinyBang.Ast
import Language.TinyBang.Display
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
    
instance Display InterpreterError where
  makeDoc ierr = case ierr of
    LexerFailure msg -> text "Lexer error:" <+> text msg
    ParserFailure msg -> text "Parser error:" <+> text msg
    -- TODO: typechecking case
    EvaluationFailure evalErr _ -> text "Evaluation error:" <+> case evalErr of
      IllFormedExpression ill -> text "Ill-formed expression:" <+> case ill of
        DuplicateFlowBinding x -> text "Duplicate flow variable binding:"
                                  <+> makeDoc x
        DuplicateFlowUse x -> text "Duplicate flow variable use:"
                              <+> makeDoc x
        DuplicateCellBinding y -> text "Duplicate cell variable binding:"
                                  <+> makeDoc y
        InvalidExpressionEnd cl -> text "Invalid ending clause for expression:"
                                   <+> makeDoc cl
        EmptyExpression -> text "Empty subexpression"
      FlowVarNotClosed x -> text "Flow variable not closed:" <+> makeDoc x
      CellVarNotClosed y -> text "Cell variable not closed:" <+> makeDoc y
      ProjectionFailure x proj -> text "Could not project" <+> makeDoc proj
                                      <+> text "from" <+> makeDoc x
      ApplicationFailure x1 x2 -> text "Could not apply" <+> makeDoc x1
                                      <+> text "to" <+> makeDoc x2
    EvaluationDisabled -> text "(evaluation disabled)"
    
-- |Interprets the provided String as a TinyBang expression.  This routine is
--  similar to @interpretSource@ from
--  @Executables.Interpreter.SourceInterpreter@ but converts the result to a
--  user-readable string.
stringyInterpretSource :: InterpreterConfiguration -> String -> String
stringyInterpretSource interpConf exprSrc =
  case interpretSource interpConf exprSrc of
    Left err -> display err
    Right (InterpreterResult x fvs cvs) ->
      "*** Flow variables: \n" ++
      display fvs ++ "\n" ++
      "*** Cell variables: \n" ++
      display cvs ++ "\n" ++
      "*** Result variable: " ++
      display x
