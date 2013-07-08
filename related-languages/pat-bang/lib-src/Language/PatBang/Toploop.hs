{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs #-}

module Language.PatBang.Toploop
( stringyInterpretSource
, interpretSource
, InterpreterConfiguration(..)
, ConstraintDatabaseType(..)
) where

import Control.Monad
import Data.Map (Map)

import Language.PatBang.Ast
import Language.PatBang.Display
import Language.PatBang.Interpreter as I
import Language.PatBang.Interpreter.DeepValues
import Language.PatBang.Syntax.Lexer
import Language.PatBang.Syntax.Location
import Language.PatBang.Syntax.Parser
import Language.PatBang.TypeSystem.Closure
import Language.PatBang.TypeSystem.ConstraintDatabase as CDb
import Language.PatBang.TypeSystem.ConstraintDatabase.Simple
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Inconsistency as In
import Language.PatBang.TypeSystem.InitialDerivation as ID
import Language.PatBang.TypeSystem.Relations
import Language.PatBang.TypeSystem.TypeInference as TI

data InterpreterError db
  = LexerFailure String
  | ParserFailure String
  | TypecheckFailure (TypecheckingError db)
  | EvaluationFailure EvalError [Clause]
  | EvaluationDisabled

data InterpreterResult
  = InterpreterResult
      -- |The variable containing the result of computation.
      FlowVar
      -- |The mapping from flow variables to their values.
      (Map FlowVar Value)
      -- |The mapping from cell variables to their contents.
      (Map CellVar FlowVar)
  deriving (Eq, Ord, Show)
  
data ConstraintDatabaseType
  = Simple
  
data DummyDatabase where
  DummyDatabase :: forall db. (ConstraintDatabase db, Display db, Ord db)
                => db -> DummyDatabase
  
data InterpreterConfiguration
  = InterpreterConfiguration
    { typechecking :: Bool
    , evaluating :: Bool
    , databaseType :: ConstraintDatabaseType }
  
-- |Interprets the provided String as a PatBang expression.  This value will be
--  lexed, parsed, typechecked, and executed.  This function takes a dummy
--  constraint database to drive the database type used by type inference.
interpretSource :: (ConstraintDatabase db, Display db, Ord db)
                => db -> InterpreterConfiguration
                -> String -> Either (InterpreterError db) InterpreterResult
interpretSource _ interpConf src = do
  tokens <- doStep LexerFailure $ lexPatBang "<stdin>" src
  ast <- doStep ParserFailure $ parsePatBang 
              ParserContext { contextDocument = UnknownDocument
                            , contextDocumentName = "<stdin>" }
              tokens
  when (typechecking interpConf) $
    void $ doStep TypecheckFailure $ typecheck ast
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
    
instance (ConstraintDatabase db, Display db)
      => Display (InterpreterError db) where
  makeDoc ierr = case ierr of
    LexerFailure msg -> text "Lexer error:" <+> text msg
    ParserFailure msg -> text "Parser error:" <+> text msg
    TypecheckFailure typeFail -> text "Type error:" <+> case typeFail of
      InitialDerivationFailed initDerivErr -> case initDerivErr of
        ID.IllFormedExpression ill -> text "Ill-formed expression:" <+>
          case ill of
            DuplicateFlowBinding x -> text "Duplicate variable definition:"
              <+> makeDoc x
            DuplicateFlowUse x -> text "Duplicate variable use:"
              <+> makeDoc x
            DuplicateCellBinding y -> text "Duplicate variable definition:"
              <+> makeDoc y
            InvalidExpressionEnd cl ->
              text "Invalid clause terminating expression:" <+> makeDoc cl
            EmptyExpression ->
              text "Source contains an empty expression"
        ID.OpenExpression vs -> text "Open variables in expression:" <+>
          makeDoc vs
      ClosureFailed (ClosureFailedProjection projErr) -> msgForProjErr projErr
      InconsistencyFailed projErr -> msgForProjErr projErr
      ClosureInconsistent incons db ->
        text "Database" </> nest 2 (makeDoc db) </> text "has inconsistencies:"
        <> nest 2 (linebreak <> nest 2 (foldr1 (<$$>) $ map docForIncon incons))
    EvaluationFailure evalErr _ -> text "Evaluation error:" <+> case evalErr of
      I.IllFormedExpression ill -> text "Ill-formed expression:" <+> case ill of
        DuplicateFlowBinding x -> text "Duplicate flow variable binding:"
                                  <+> makeDoc x
        DuplicateFlowUse x -> text "Duplicate flow variable use:"
                              <+> makeDoc x
        DuplicateCellBinding y -> text "Duplicate cell variable binding:"
                                  <+> makeDoc y
        InvalidExpressionEnd cl -> text "Invalid ending clause for expression:"
                                   <+> makeDoc cl
        EmptyExpression -> text "Empty subexpression"
      I.OpenExpression vs -> text "Open variables in expression:" <+> makeDoc vs
      FlowVarNotClosed x -> text "Flow variable not closed:" <+> makeDoc x
      CellVarNotClosed y -> text "Cell variable not closed:" <+> makeDoc y
      ProjectionFailure x proj -> text "Could not project" <+> makeDoc proj
                                      <+> text "from" <+> makeDoc x
      I.ApplicationFailure x1 x2 -> text "Could not apply" <+> makeDoc x1
                                      <+> text "to" <+> makeDoc x2
    EvaluationDisabled -> text "(evaluation disabled)"
    where
      msgForProjErr (NonContractiveType proj typ vars) =
        text "Non-contractive type projecting" <+> makeDoc proj
          <+> text "with type" <+> makeDoc typ <+> text "through variables"
          <+> makeDoc vars
      docForIncon :: (ConstraintDatabase db, Display db)
                  => Inconsistency db -> Doc
      docForIncon incon = case incon of
        In.ApplicationFailure ac _
            (ApplicationCompatibilityResult arg scapes _ fib) ->
          text "Application failure:" </> nest 2
            (
              text "At application site" <+> makeDoc ac </>
              text "we have scapes" <+> makeDoc scapes </>
              text "and argument" <+> makeDoc arg <+>
              text "but fibration" <+> makeDoc fib </>
              text "produces a type which does not apply."
            )
        In.IntegerOperationFailure oc _ _ ->
          text "Integer operation failure at " <+> makeDoc oc

-- |Interprets the provided String as a PatBang expression.  This routine is
--  similar to @interpretSource@ but converts the result to a user-readable
--  string.  This function takes a dummy constraint database to drive the
--  database type used by type inference.
stringyInterpretSource :: InterpreterConfiguration -> String -> String
stringyInterpretSource interpConf exprSrc =
  case emptyDatabaseFromType $ databaseType interpConf of
    DummyDatabase dummy ->
      case interpretSource dummy interpConf exprSrc of
        Left err -> display err
        Right (InterpreterResult x fvs cvs) ->
          case deepOnion fvs cvs x of
            Left failure -> case failure of
              UnboundFlowVariable ux ->
                "Unbound flow variable " ++ display ux ++ " in result!"
              UnboundCellVariable uy ->
                "Unbound cell variable " ++ display uy ++ " in result!"
            Right value -> display value

-- |Creates an empty database of a recognized type.
emptyDatabaseFromType :: ConstraintDatabaseType -> DummyDatabase
emptyDatabaseFromType dbt = case dbt of
  Simple -> DummyDatabase (CDb.empty :: SimpleConstraintDatabase)
