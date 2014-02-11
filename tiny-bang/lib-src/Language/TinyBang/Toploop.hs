{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs #-}

module Language.TinyBang.Toploop
( stringyInterpretSource
, interpretSource
, interpretAst
, InterpreterConfiguration(..)
, InterpreterResult(..)
, ConstraintDatabaseType(..)
, emptyDatabaseFromType
) where

import Control.Monad
import Data.Either.Combinators
import Data.List
import Data.Map (Map)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import qualified Language.TinyBang.Interpreter as I
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Parser
import qualified Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.Constraints
import qualified Language.TinyBang.TypeSystem.InitialDerivation as ID
import qualified Language.TinyBang.TypeSystem.TypeInference as TI
import Language.TinyBang.Utils.Display

data InterpreterError db
  = LexerFailure String
  | ParserFailure String
  | TypecheckFailure (TI.TypecheckingError db)
  | EvaluationFailure I.EvalError [Clause]
  | EvaluationDisabled

data InterpreterResult
  = InterpreterResult
      -- |The variable containing the result of computation.
      Var
      -- |The mapping from flow variables to their values.
      (Map Var Value)
  deriving (Eq, Ord, Show)

instance Display InterpreterResult where
  makeDoc (InterpreterResult x vs) =
    case deepOnion vs x of
      Left failure -> case failure of
        UnboundVariable ux -> text "Unbound variable" <+> makeDoc ux <+>
                                text "in result!"
      Right value -> makeDoc value

-- TODO: perhaps this should go somewhere else.  The unit tests would like to
--       use these as well and unit tests shouldn't need the toploop, right?
data ConstraintDatabaseType
  = Simple
  | Indexed
  deriving (Show)

data InterpreterConfiguration
  = InterpreterConfiguration
    { typechecking :: Bool
    , evaluating :: Bool
    , databaseType :: ConstraintDatabaseType
    }
  
-- |Interprets the provided String as a TinyBang expression.  This value will be
--  lexed, parsed, typechecked, and executed.  This function takes a dummy
--  constraint database to drive the database type used by type inference.
interpretSource :: (CDb.ConstraintDatabase db, Display db)
                => db -> InterpreterConfiguration
                -> String -> Either (InterpreterError db) InterpreterResult
interpretSource dummy interpConf src = do
  let doc = UnknownDocument
  tokens <- mapLeft LexerFailure $ lexTinyBang doc src
  ast <- mapLeft ParserFailure $ parseTinyBang doc tokens
  interpretAst dummy interpConf ast

-- |Interprets the provided TinyBang AST.
interpretAst :: (CDb.ConstraintDatabase db, Display db)
                => db -> InterpreterConfiguration
                -> Expr -> Either (InterpreterError db) InterpreterResult
interpretAst _ interpConf ast = do
  when (typechecking interpConf) $
    void $ mapLeft TypecheckFailure $ TI.typecheck ast
  if evaluating interpConf
    then do
      (env,var) <- mapLeft (uncurry EvaluationFailure) $ I.eval ast
      return $ InterpreterResult var (I.varMap env)
    else
      Left EvaluationDisabled
    
instance (Display db) => Display (InterpreterError db) where
  makeDoc ierr = case ierr of
    LexerFailure msg -> text "Lexer error:" <+> text msg
    ParserFailure msg -> text "Parser error:" <+> text msg
    TypecheckFailure typeFail -> text "Type error:" <+> case typeFail of
      TI.InitialDerivationFailed initDerivErr -> case initDerivErr of
        ID.IllFormedExpression ills -> text "Ill-formed expression:" </>
          indent 2 (align $ foldl1 (</>) $ flip map (Set.toList ills) $ \ill ->
            case ill of
              DuplicateDefinition x ->
                text "Duplicate variable definition:" <+> makeDoc x
              OpenExpression xs ->
                text "Expression is open in the variables" <+> makeDoc xs
              EmptyExpression o ->
                text "Source contains an empty expression at" <+> makeDoc o
              EmptyPattern o ->
                text "Source contains an empty pattern at" <+> makeDoc o
            )
      TI.ClosureInconsistent incons db ->
        text "Database" </> nest 2 (makeDoc db) </> text "has inconsistencies:"
        <> nest 2 (linebreak <> nest 2 (foldr1 (<$$>) $ map docForIncon $
                                          Set.toList incons))
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
      I.BuiltinBadOperandCount o bop n n' ->
        text "At" <+> makeDoc o <> char ',' <+> text "builtin operation " <+>
          makeDoc bop <+> text "had an invalid operand count (had" <+>
          text (show n) <> char ',' <+> text "expected" <+> text (show n') <>
          text ")"
      I.BuiltinBadOperandType o bop n x ->
        text "At" <+> makeDoc o <> char ',' <+> text "builtin operation " <+>
          makeDoc bop <+> text "had an invalid operand type in position" <+>
          text (show n) <+> text "(variable" <+> makeDoc x <+> text ")"
    EvaluationDisabled -> text "(evaluation disabled)"
    where
      docForIncon :: (Display db) => Inconsistency db -> Doc
      docForIncon incon = case incon of
        Language.TinyBang.TypeSystem.Constraints.ApplicationFailure
            sa aa csa as ->
          text "Application failure:" </> nest 2
            (
              text "At application site" <+> makeDoc csa </>
              text "we have scape variable" <+> makeDoc sa </>
              text "and argument variable" <+> makeDoc aa <+>
              text "but argument slice" <+> makeDoc as </>
              text "does not apply."
            )
        Language.TinyBang.TypeSystem.Constraints.BuiltinBadOperandCount
            site op expected actual ->
          text "Bad built-in operand count:" </> nest 2
            (
              text "At site" <+> makeDoc site <> text "," <+> makeDoc actual <+>
              text "operands appeared for operator" <+> makeDoc op <+>
              text "when" <+> makeDoc expected <+>
              text "operands were expected"
            )
        Language.TinyBang.TypeSystem.Constraints.BuiltinBadOperandType
            site op index a ->
          text "Bad built-in operand type:" </> nest 2
            ( text "At site" <+> makeDoc site <> text "," <+> text "operand" <+>
              makeDoc index <+> text "for operator" <+> makeDoc op <+>
              text "had an incorrect type (original variable" <+> makeDoc a <>
              char ')'
            )

-- |Interprets the provided String as a TinyBang expression.  This routine is
--  similar to @interpretSource@ but converts the result to a user-readable
--  string.  This function takes a dummy constraint database to drive the
--  database type used by type inference.
stringyInterpretSource :: InterpreterConfiguration -> String -> String
stringyInterpretSource interpConf exprSrc =
  case emptyDatabaseFromType $ databaseType interpConf of
    CDb.SomeDisplayableConstraintDatabase dummy ->
      case interpretSource dummy interpConf exprSrc of
        Left err -> display err
        Right result -> display result

-- |Creates an empty database of a recognized type.
emptyDatabaseFromType :: ConstraintDatabaseType
                      -> CDb.SomeDisplayableConstraintDatabase
emptyDatabaseFromType dbt = case dbt of
  Simple ->
    CDb.SomeDisplayableConstraintDatabase
      (CDb.empty :: CDb.SimpleConstraintDatabase)
  Indexed ->
    CDb.SomeDisplayableConstraintDatabase
      (CDb.empty :: CDb.IndexedConstraintDatabase)

