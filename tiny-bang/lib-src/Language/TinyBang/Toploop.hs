{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs #-}

module Language.TinyBang.Toploop
( stringyInterpretSource
, interpretSource
, InterpreterConfiguration(..)
, ConstraintDatabaseType(..)
) where

import Data.List
import Data.Map (Map)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display
import Language.TinyBang.Interpreter as I
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Parser
import Language.TinyBang.TypeSystem.Closure
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Inconsistency as In
import Language.TinyBang.TypeSystem.InitialDerivation as ID
import Language.TinyBang.TypeSystem.TypeInference as TI

-- TODO: restore typechecking stuff

data InterpreterError db
  = LexerFailure String
  | ParserFailure String
--  | TypecheckFailure (TypecheckingError db)
  | EvaluationFailure EvalError [Clause]
  | EvaluationDisabled

data InterpreterResult
  = InterpreterResult
      -- |The variable containing the result of computation.
      Var
      -- |The mapping from flow variables to their values.
      (Map Var Value)
  deriving (Eq, Ord, Show)

-- TODO: perhaps this should go somewhere else.  The unit tests would like to
--       use these as well and unit tests shouldn't need the toploop, right?
data ConstraintDatabaseType
  = Simple

data DummyDatabase where
  DummyDatabase :: forall db. (ConstraintDatabase db, Display db)
                => db -> DummyDatabase
  
data InterpreterConfiguration
  = InterpreterConfiguration
    { typechecking :: Bool
    , evaluating :: Bool
    , databaseType :: ConstraintDatabaseType }
  
-- |Interprets the provided String as a TinyBang expression.  This value will be
--  lexed, parsed, typechecked, and executed.  This function takes a dummy
--  constraint database to drive the database type used by type inference.
interpretSource :: (ConstraintDatabase db, Display db)
                => db -> InterpreterConfiguration
                -> String -> Either (InterpreterError db) InterpreterResult
interpretSource _ interpConf src = do
  let doc = UnknownDocument
  tokens <- doStep LexerFailure $ lexTinyBang doc src
  ast <- doStep ParserFailure $ parseTinyBang doc tokens
  {- TODO: replace
  when (typechecking interpConf) $
    void $ doStep TypecheckFailure $ typecheck ast
  -}
  if evaluating interpConf
    then do
      (env,var) <- doStep evalFail $ eval ast
      return $ InterpreterResult var (varMap env)
    else
      Left EvaluationDisabled
  where
    doStep errConstr computation =
      case computation of
        Left err -> Left $ errConstr err
        Right ans -> Right ans
    evalFail (err,cls) = EvaluationFailure err cls
    
instance (Display db) => Display (InterpreterError db) where
  makeDoc ierr = case ierr of
    LexerFailure msg -> text "Lexer error:" <+> text msg
    ParserFailure msg -> text "Parser error:" <+> text msg
    {- TODO: replace
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
    -}
    EvaluationFailure evalErr _ -> text "Evaluation error:" <+> case evalErr of
      I.IllFormedExpression ills -> text "Ill-formed expression:" </>
        indent 2 (align $ foldl1 (</>) $
          map (\ill -> case ill of
            DuplicateDefinition x -> text "Duplicate variable definition:"
                                      <+> makeDoc x
            OpenExpression xs -> text "Expression is open in variables: "
                                      <+> foldl1 (<>) (intersperse (char ',') $
                                            map makeDoc $ Set.toList xs)
            EmptyExpression o -> text "Empty expression at " <+> makeDoc o
            EmptyPattern o -> text "Empty pattern at " <+> makeDoc o
            ) $ Set.toList ills)
      I.ApplicationFailure x1 x2 -> text "Could not apply" <+> makeDoc x1
                                      <+> text "to" <+> makeDoc x2
    EvaluationDisabled -> text "(evaluation disabled)"
    {- TODO: replace
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
    -}

-- |Interprets the provided String as a TinyBang expression.  This routine is
--  similar to @interpretSource@ but converts the result to a user-readable
--  string.  This function takes a dummy constraint database to drive the
--  database type used by type inference.
stringyInterpretSource :: InterpreterConfiguration -> String -> String
stringyInterpretSource interpConf exprSrc =
  case emptyDatabaseFromType $ databaseType interpConf of
    DummyDatabase dummy ->
      case interpretSource dummy interpConf exprSrc of
        Left err -> display err
        Right (InterpreterResult x vs) ->
          case deepOnion vs x of
            Left failure -> case failure of
              UnboundVariable ux ->
                "Unbound variable " ++ display ux ++ " in result!"
            Right value -> display value

-- |Creates an empty database of a recognized type.
emptyDatabaseFromType :: ConstraintDatabaseType -> DummyDatabase
emptyDatabaseFromType dbt = case dbt of
  Simple ->
    DummyDatabase (CDb.empty :: SimpleConstraintDatabase)
