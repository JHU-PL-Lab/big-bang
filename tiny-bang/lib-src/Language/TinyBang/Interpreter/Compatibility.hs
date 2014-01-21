{-# LANGUAGE TupleSections, TemplateHaskell #-}

{-|
  A module containing the compatibility relations for evaluation.
-}

module Language.TinyBang.Interpreter.Compatibility
( compatibility
) where

import Control.Monad
import Data.Maybe
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Utils.Display
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

-- |Calculates the compatibility of the first variable in the environment with
--  the pattern expressed by the second variable and its pattern structure.  The
--  result is @Nothing@ when they are incompatible and @Just@ an expression of
--  the appropriate bindings when they are compatible.
compatibility :: Var -> Var -> Pattern -> EvalM (Maybe Expr)
compatibility x x' pat =
  bracketLogM _debugI
    (display $ text "Checking compatibility of " <+> makeDoc x <+>
      text " with " <+> makeDoc x' <+> text " under " <+> makeDoc pat)
    (\result ->
      case result of
        Just expr ->
          display $ text "Variable " <+> makeDoc x <+>
            text " is compatible with " <+> makeDoc x' <+> text " under " <+>
            makeDoc pat <+> text " as expression " <+> makeDoc expr
        Nothing ->
          display $ text "Variable " <+> makeDoc x <+>
            text " is not compatible with " <+> makeDoc x' <+>
            text " under " <+> makeDoc pat)
    $ do
        v <- varLookup x
        pv <- patVarLookup pat x'
        case pv of
          PPrimitive _ primt ->
            case v of
              VOnion _ x1 x2 -> matchOnOnion x1 x2
              VPrimitive _ prim | typeOfPrimitiveValue prim == primt ->
                return $ Just $ Expr generated []
              _ -> return Nothing
          PEmptyOnion _ ->
            return $ Just $
              Expr generated [RedexDef generated x' $ Define generated x]
          PLabel _ n x1' ->
            case v of
              VOnion _ x1 x2 -> matchOnOnion x1 x2
              VLabel _ n' x1 | n == n' -> compatibility x1 x1' pat
              _ -> return Nothing
          PRef _ x1' ->
            case v of
              VOnion _ x1 x2 -> matchOnOnion x1 x2
              VRef _ x1 -> compatibility x1 x1' pat
              _ -> return Nothing
          PConjunction _ x1' x2' -> do
            me1 <- compatibility x x1' pat
            me2 <- compatibility x x2' pat
            return $ liftM2 exprConcat me1 me2
  where
    matchOnOnion :: Var -> Var -> EvalM (Maybe Expr)
    matchOnOnion x1 x2 = do
      me1 <- compatibility x1 x' pat
      if isNothing me1 then compatibility x2 x' pat else return me1
    patVarLookup :: Pattern -> Var -> EvalM PatternValue
    patVarLookup (Pattern _ pcls) px =
      let pvs = mapMaybe (\(PatternClause _ px' pv) ->
                                if px == px' then Just pv else Nothing) pcls in
      case pvs of
        [] -> raiseEvalError $ IllFormedExpression $ Set.singleton $
                                  OpenExpression $ Set.singleton px
        [pv] -> return pv
        _ -> raiseEvalError $ IllFormedExpression $ Set.singleton $
                                DuplicateDefinition px
