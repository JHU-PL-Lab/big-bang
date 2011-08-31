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

-- |A result type for evalStringTop
data EvalStringResult
    = EvalSuccess A.Expr
    | EvalFailure I.EvalError
    | Contradiction T.Constraints
        -- ^Represents a contradiction appearing in a constraint set.  The
        --  indicated set should contain at least one contradiction.
    | TypecheckFailure TI.TypeInferenceError T.Constraints
    | ParseFailure -- TODO: add some payload here
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
        Right answer -> EvalSuccess answer

eAll :: String -> EvalStringM A.Expr
eAll s = do
    tokens <- eLex s
    ast <- eParse tokens
    (_,cs) <- eTypeInfer ast
    _ <- eClose cs
    val <- eEval ast
    return val

eLex :: String -> EvalStringM [L.Token]
eLex s =
    case L.lexLittleBang s of
        Left err -> throwError $ FailureWrapper $ LexFailure err
        Right tokens -> return tokens

eParse :: [L.Token] -> EvalStringM A.Expr
eParse tokens =
    -- TODO: case this out once parser is monadic
    let ast = P.parseLittleBang tokens in
    return ast

eTypeInfer :: A.Expr -> EvalStringM (T.TauDownClosed, T.Constraints)
eTypeInfer e =
    let (res,cs) = TI.inferTypeTop e in
    case res of
        Left err -> throwError $ FailureWrapper $
                TypecheckFailure err cs
        Right t -> return (t,cs)

eClose :: T.Constraints -> EvalStringM T.Constraints
eClose cs =
    let closed = C.calculateClosure cs in
    if Set.null $ Set.filter isBottom closed
        then throwError $ FailureWrapper $ Contradiction closed
        else return closed
  where isBottom c = case c of
            T.Bottom _ -> True
            _ -> False

eEval :: A.Expr -> EvalStringM A.Expr
eEval e =
    case I.evalTop e of
        Left err -> throwError $ FailureWrapper $ EvalFailure err
        Right v -> return v

instance Display EvalStringResult where
    makeDoc esr = case esr of
        EvalSuccess e -> makeDoc e
        EvalFailure err -> makeDoc err
        Contradiction cs -> text "Contradiction: " $$ (nest 4 $ makeDoc cs)
        TypecheckFailure err _ -> makeDoc err
        ParseFailure -> text "parse failure" -- TODO
        LexFailure msg -> text msg

