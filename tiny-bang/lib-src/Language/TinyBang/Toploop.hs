{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs #-}

module Language.TinyBang.Toploop
( stringyInterpretSource
, interpretSource
, interpretAst
, InterpreterConfiguration(..)
, InterpreterResult(..)
, InterpreterError(..)
) where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Either.Combinators
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import qualified Language.TinyBang.Interpreter as I
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Utils.Syntax.Location
import Language.TinyBang.Syntax.Parser
import qualified Language.TinyBang.TypeSystem as TS
import Language.TinyBang.Utils.Display

data InterpreterError
  = LexerFailure String
  | ParserFailure String
  | IllFormednessFailure (Set IllFormedness)
  | TypecheckFailure (Set TS.TypecheckError)
  | EvaluationFailure I.EvalError [Clause]
  | EvaluationDisabled
  | OtherFailure String

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

data InterpreterConfiguration
  = InterpreterConfiguration
    { typeSystem :: Maybe TS.TypeSystem
    , evaluating :: Bool
    }
  
-- |Interprets the provided String as a TinyBang expression.  This value will be
--  lexed, parsed, typechecked, and executed.
interpretSource :: InterpreterConfiguration -> String
                -> EitherT InterpreterError IO InterpreterResult
interpretSource interpConf src = do
  let doc = UnknownDocument
  tokens <- hoistEither $ mapLeft LexerFailure $ lexTinyBang doc src
  ast <- hoistEither $ mapLeft ParserFailure $ parseTinyBang doc tokens
  interpretAst interpConf ast

-- |Interprets the provided TinyBang AST.
interpretAst :: InterpreterConfiguration -> Expr
             -> EitherT InterpreterError IO InterpreterResult
interpretAst interpConf ast = do
  let ills = checkWellFormed ast
  unless (Set.null ills) $ left $ IllFormednessFailure ills
  case typeSystem interpConf of
    Just ts -> do
      let tsResult = TS.typecheck ts ast
      let errs = TS.typeErrors tsResult
      unless (Set.null errs) $ left $ TypecheckFailure errs
    Nothing ->
      return ()
  if evaluating interpConf
    then
      let transErr = uncurry EvaluationFailure in
      let transVal (env,var) = InterpreterResult var (I.varMap env) in
      bimapEitherT transErr transVal $ I.eval ast
    else
      left EvaluationDisabled
    
instance Display InterpreterError where
  makeDoc ierr = case ierr of
    LexerFailure msg -> text "Lexer error:" <+> text msg
    ParserFailure msg -> text "Parser error:" <+> text msg
    IllFormednessFailure ills -> text "Ill-formed input expression:" </>
      indent 2 (align $ foldl1 (</>) $ map makeDoc (Set.toList ills))
    TypecheckFailure typeFail -> undefined -- TODO
      {-
      text "Type error:" <+> case typeFail of
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
            OpenPattern xs -> text "Pattern is open in variables: "
                                      <+> foldl1 (<>) (intersperse (char ',') $
                                            map makeDoc $ Set.toList xs)
            EmptyExpression o -> text "Empty expression at " <+> makeDoc o
            RefPatternNotExactlyEmpty x ->
              text "Ref pattern contents not empty for variable" <+> makeDoc x
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
    OtherFailure oFailure -> text oFailure
    EvaluationDisabled -> text "(evaluation disabled)"
    where
      docForIncon :: TS.Inconsistency -> Doc
      docForIncon incon = undefined -- TODO
        {- case incon of
        Language.TinyBang.TypeSystem.Constraints.ApplicationFailure
            sa aa csa as ->
          text "Application failure:" </> nest 2
            (
              text "At application site" <+> makeDoc csa </>
              text "we have scape variable" <+> makeDoc sa </>
              text "and argument variable" <+> makeDoc aa </>
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
            -}

-- |Interprets the provided String as a TinyBang expression.  This routine is
--  similar to @interpretSource@ but converts the result to a user-readable
--  string.  This function takes a dummy constraint database to drive the
--  database type used by type inference.
stringyInterpretSource :: InterpreterConfiguration -> String -> IO String
stringyInterpretSource interpConf exprSrc = do
  res <- runEitherT $ interpretSource interpConf exprSrc
  case res of
    Left err -> return $ display err
    Right result -> return $ display result
