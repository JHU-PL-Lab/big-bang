module Language.TinyBang.Interpreter.SourceInterpreter
( evalStringTop
, EvalStringResult(..)
, EvalSuccessOrFailure(..)
) where

import Control.Monad.Error (Error, strMsg, throwError)
import qualified Data.Set as Set
import Data.IntMap (IntMap)

import Language.TinyBang.Render.Display
import qualified Language.TinyBang.Ast as A
import qualified Language.TinyBang.Interpreter.Interpreter as I
import qualified Language.TinyBang.Syntax.Lexer as L
import qualified Language.TinyBang.Syntax.Parser as P
import qualified Language.TinyBang.Types.Closure as C
import qualified Language.TinyBang.Types.TypeInference as TI
import qualified Language.TinyBang.Types.Types as T

import Debug.Trace

data EvalSuccessOrFailure = EvalSuccess (A.Value, IntMap A.Value) | EvalFailure I.EvalError

-- |A result type for evalStringTop
data EvalStringResult
    = EvalResult A.Expr EvalSuccessOrFailure
    | Contradiction A.Expr T.Constraints
        -- ^Represents a contradiction appearing in a constraint set.  The
        --  indicated set should contain at least one contradiction.
    | TypecheckFailure A.Expr TI.TypeInferenceError T.Constraints
    | ParseFailure P.ParseError
    | LexFailure String

-- |A monadic exception type for the evalStringTop process.
data EvalStringResultErrorWrapper =
    FailureWrapper EvalStringResult
instance Error EvalStringResultErrorWrapper where
    strMsg = error

type EvalStringM a = Either EvalStringResultErrorWrapper a

-- |Performs top-level evaluation of a Big Bang source string.  This routine is
--  provided for convenience and testing.  It attempts to lex, parse, typecheck
--  and evaluate the code, producing an appropriate result.
evalStringTop :: String -> EvalStringResult
evalStringTop s =
    let result = eAll s in
    case result of
        Left (FailureWrapper err) -> err
        Right runnableAst -> EvalResult runnableAst $ eEval runnableAst

eAll :: String -> EvalStringM A.Expr
eAll s = do
    tokens <- eLex s
    ast <- eParse tokens
    (_,cs) <- eTypeInfer ast
    _ <- eClose ast cs
    return ast

eLex :: String -> EvalStringM [L.Token]
eLex s =
    case L.lexTinyBang s of
        Left err -> throwError $ FailureWrapper $ LexFailure err
        Right tokens -> return tokens

eParse :: [L.Token] -> EvalStringM A.Expr
eParse tokens =
    case P.parseTinyBang tokens of
        Left err -> throwError $ FailureWrapper $ ParseFailure err
        Right ast -> return ast

eTypeInfer :: A.Expr -> EvalStringM (T.TauDownClosed, T.Constraints)
eTypeInfer e =
    let (res,cs) = TI.inferTypeTop e in
    case res of
        Left err -> throwError $ FailureWrapper $
                TypecheckFailure e err cs
        Right t -> return (t,cs)

eClose :: A.Expr -> T.Constraints -> EvalStringM T.Constraints
eClose e cs =
    let closed = C.calculateClosure cs in
    if Set.null $ Set.filter isBottom closed
        then return closed
        else throwError $ FailureWrapper $ Contradiction e closed

isBottom :: T.Constraint -> Bool
isBottom c = case c of
    T.Bottom _ -> True
    _ -> False

eEval :: A.Expr -> EvalSuccessOrFailure
eEval e =
    case I.evalTop e of
        Left err -> EvalFailure err
        Right v -> EvalSuccess v

instance Display EvalStringResult where
    makeDoc esr = case esr of
        EvalResult ast res -> makeDoc ast $$ makeDoc res
        Contradiction ast cs -> text "Contradiction: " $$
                               (nest 4 $ makeDoc ast $$
                                         makeDoc (head . filter isBottom . Set.toList $ cs))
        TypecheckFailure ast err _ -> makeDoc ast $$ makeDoc err
        ParseFailure err -> makeDoc err
        LexFailure msg -> text msg

instance Display EvalSuccessOrFailure where
    makeDoc res = case res of
        EvalSuccess e -> makeDoc e
        EvalFailure err -> makeDoc err