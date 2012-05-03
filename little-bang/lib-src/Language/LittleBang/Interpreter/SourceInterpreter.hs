{-# LANGUAGE ImplicitParams #-}
module Language.LittleBang.Interpreter.SourceInterpreter
( evalStringTop
, EvalStringResult(..)
, EvalSuccessOrFailure(..)
, I.canonicalize
, I.onion
) where

-- TODO: much of this logic is painfully similar to the Tiny Bang source
-- interpreter.  Is there a sensible or meaningful abstraction to use
-- here?  Lexing and parsing are different and translation is distinct,
-- but the rest of the steps are the same.

import Control.Monad (when)
import Control.Monad.Error (Error, strMsg, throwError)
import qualified Data.Set as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Language.LittleBang.Ast as LA
import qualified Language.LittleBang.Syntax.Lexer as L
import qualified Language.LittleBang.Syntax.Parser as P
import Language.LittleBang.Translator (convertLittleToTiny)

import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Config as Cnf
import qualified Language.TinyBang.Interpreter.Ast as IA
import qualified Language.TinyBang.Interpreter.Interpreter as I
import qualified Language.TinyBang.Types.Closure as C
import qualified Language.TinyBang.Types.TypeInference as TI
import qualified Language.TinyBang.Types.Types as T

import Utils.Render.Display

data EvalSuccessOrFailure
    = EvalSuccess (TA.Value IA.Expr, IntMap (TA.Value IA.Expr))
    | EvalFailure (I.EvalError IA.Expr)

-- |A result type for evalStringTop
data EvalStringResult
    = EvalResult TA.Expr EvalSuccessOrFailure
    | Contradiction TA.Expr T.Constraints
        -- ^Represents a contradiction appearing in a constraint set.  The
        --  indicated set should contain at least one contradiction.
    | TypecheckFailure TA.Expr TI.TypeInferenceError T.Constraints
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
evalStringTop :: (?conf :: Cnf.Config) => String -> EvalStringResult
evalStringTop s =
    let result = eAll s in
    case result of
        Left (FailureWrapper err) -> err
        Right ast -> EvalResult ast $
            if Cnf.evaluating then eEval ast
                              else EvalSuccess (TA.VEmptyOnion, IntMap.empty)

eAll :: (?conf :: Cnf.Config) => String -> EvalStringM TA.Expr
eAll s = do
    tokens <- eLex s
    lAst <- eParse tokens
    let ast = convertLittleToTiny lAst
    when Cnf.typechecking
        ( do
            (_,cs) <- eTypeInfer ast
            _ <- eClose ast cs
            return ()
        )
    return ast

eLex :: String -> EvalStringM [L.Token]
eLex s =
    case L.lexLittleBang s of
        Left err -> throwError $ FailureWrapper $ LexFailure err
        Right tokens -> return tokens

eParse :: [L.Token] -> EvalStringM LA.Expr
eParse tokens =
    case P.parseLittleBang tokens of
        Left err -> throwError $ FailureWrapper $ ParseFailure err
        Right ast -> return ast

eTypeInfer :: TA.Expr -> EvalStringM (T.InterAlpha, T.Constraints)
eTypeInfer e =
    let (res,cs) = TI.inferTypeTop e in
    case res of
        Left err -> throwError $ FailureWrapper $
                TypecheckFailure e err cs
        Right a -> return (a,cs)

eClose :: (?conf :: Cnf.Config) => TA.Expr -> T.Constraints -> EvalStringM T.Constraints
eClose e cs =
    let closed = C.calculateClosure cs in
    if Set.null $ Set.filter isBottom closed
        then return closed
        else throwError $ FailureWrapper $ Contradiction e closed

isBottom :: T.Constraint -> Bool
isBottom c = case c of
    T.Bottom _ -> True
    _ -> False

eEval :: (?conf :: Cnf.Config) => TA.Expr -> EvalSuccessOrFailure
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
