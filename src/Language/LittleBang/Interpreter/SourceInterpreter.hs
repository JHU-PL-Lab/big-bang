module Language.LittleBang.Interpreter.SourceInterpreter
( evalStringTop
, EvalStringResult(..)
) where

import Control.Monad.Error (Error, strMsg, throwError)
import qualified Data.Set as Set

import Language.LittleBang.Render.Display
import qualified Language.LittleBang.Ast as A
import qualified Language.LittleBang.Interpreter.Interpreter as I
import qualified Language.LittleBang.Syntax.Lexer as L
import qualified Language.LittleBang.Syntax.Parser as P
import qualified Language.LittleBang.Types.Closure as C
import qualified Language.LittleBang.Types.TypeInference as TI
import qualified Language.LittleBang.Types.Types as T

import Debug.Trace

-- |A result type for evalStringTop
data EvalStringResult
    = EvalSuccess A.Expr A.Expr
    | EvalFailure A.Expr I.EvalError
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
        Right answer -> uncurry EvalSuccess answer

eAll :: String -> EvalStringM (A.Expr, A.Expr)
eAll s = do
    tokens <- eLex s
    ast <- eParse tokens
    (_,cs) <- eTypeInfer ast
    _ <- eClose ast cs
    val <- eEval ast
    return (ast, val)

eLex :: String -> EvalStringM [L.Token]
eLex s =
    case L.lexLittleBang s of
        Left err -> throwError $ FailureWrapper $ LexFailure err
        Right tokens -> return tokens

eParse :: [L.Token] -> EvalStringM A.Expr
eParse tokens =
    case P.parseLittleBang tokens of
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
  where isBottom c = case c of
            T.Bottom _ -> True
            _ -> False

eEval :: A.Expr -> EvalStringM A.Expr
eEval e =
    case I.evalTop e of
        Left err -> throwError $ FailureWrapper $ EvalFailure e err
        Right v -> return v

instance Display EvalStringResult where
    makeDoc esr = case esr of
        EvalSuccess _ e -> makeDoc e
        EvalFailure ast err -> makeDoc ast $$ makeDoc err
        Contradiction ast cs -> text "Contradiction: " $$
                               (nest 4 $ makeDoc ast $$ makeDoc cs)
        TypecheckFailure ast err _ -> makeDoc ast $$ makeDoc err
        ParseFailure err -> makeDoc err
        LexFailure msg -> text msg

