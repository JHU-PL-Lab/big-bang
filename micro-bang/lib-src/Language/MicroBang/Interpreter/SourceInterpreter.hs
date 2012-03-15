module Language.MicroBang.Interpreter.SourceInterpreter
( evalStringTop) where



import Control.Monad.Error (Error, strMsg, throwError)
import qualified Data.Set as Set
import Data.IntMap (IntMap)

import qualified Language.MicroBang.Ast as A
import qualified Language.MicroBang.Interpreter.Interpreter as I
import qualified Language.MicroBang.Syntax.Lexer as L
import qualified Language.MicroBang.Syntax.Parser as P

import Language.MicroBang.Ast
  ( Expr(..)
  , Operator(..)
  , Chi(..)
  , Branches
  , Branch(..)
  , Value(..)
  , exprFromValue
  , Evaluated(..)
  )



---- |Performs top-level evaluation of a Micro Bang source string.  This routine is
--  provided for convenience and testing.  It attempts to lex, parse, typecheck
--  and evaluate the code, producing an appropriate result.
evalStringTop :: String -> I.EvalStringResult
evalStringTop s =
    let result = eAll s in
    case result of
        Left (I.FailureWrapper err) -> err
        Right runnableAst -> I.EvalResult runnableAst $ eEval runnableAst

eAll :: String -> I.EvalStringM Expr
eAll s = do
    tokens <- eLex s
    ast <- eParse tokens
    return ast

eLex :: String -> I.EvalStringM [L.Token]
eLex s =
    case L.lexMicroBang s of
        Left err -> throwError $ I.FailureWrapper $ I.LexFailure err
        Right tokens -> return tokens

eParse :: [L.Token] -> I.EvalStringM Expr
eParse tokens =
    case P.parseMicroBang tokens of
        Left err -> throwError $ I.FailureWrapper $ I.ParseFailure err
        Right ast -> return ast

eEval :: Expr -> I.EvalSuccessOrFailure
eEval e =
    case I.evalTop e of
        Left err -> I.EvalFailure err
        Right v -> I.EvalSuccess v