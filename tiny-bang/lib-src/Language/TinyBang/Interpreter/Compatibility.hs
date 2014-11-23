{-# LANGUAGE TupleSections, TemplateHaskell #-}

{-|
  A module containing the compatibility relations for evaluation.
-}

module Language.TinyBang.Interpreter.Compatibility
( compatibility
) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
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
compatibility :: Var -> Var -> PatternFilterMap -> EvalM (Maybe Expr)
compatibility x x' pfm =
  bracketLogM _debugI
    (display $ text "Checking compatibility of " <+> makeDoc x <+>
      text " with " <+> makeDoc x' <+> text " under " <+> makeDoc pfm)
    (\result ->
      case result of
        Just expr ->
          display $ text "Variable " <+> makeDoc x <+>
            text " is compatible with " <+> makeDoc x' <+> text " under " <+>
            makeDoc pfm <+> text " as expression " <+> makeDoc expr
        Nothing ->
          display $ text "Variable " <+> makeDoc x <+>
            text " is not compatible with " <+> makeDoc x' <+>
            text " under " <+> makeDoc pfm)
    $ do
        v <- varLookup x
        case Map.lookup x' $ unPatternFilterMap pfm of
          Nothing -> raiseEvalError $ IllFormedExpression $
                      Set.singleton $ OpenPattern $ Set.singleton x'
          Just (_,pv) ->
            case pv of
              FPrimitive _ primt ->
                case v of
                  VOnion _ x1 x2 -> matchOnOnion x1 x2
                  VPrimitive _ prim | typeOfPrimitiveValue prim == primt ->
                    return $ Just $ Expr generated []
                  _ -> return Nothing
              FEmptyOnion _ ->
                return $ Just $
                  Expr generated [Clause generated x' $ Copy generated x]
              FLabel _ n x1' ->
                case v of
                  VOnion _ x1 x2 -> matchOnOnion x1 x2
                  VLabel _ n' x1 | n == n' -> compatibility x1 x1' pfm
                  _ -> return Nothing
              FRef _ x1' ->
                case v of
                  VOnion _ x1 x2 -> matchOnOnion x1 x2
                  VRef _ x1 -> compatibility x1 x1' pfm
                  _ -> return Nothing
              FConjunction _ x1' x2' -> do
                me1 <- compatibility x x1' pfm
                me2 <- compatibility x x2' pfm
                return $ liftM2 exprConcat me1 me2
  where
    matchOnOnion :: Var -> Var -> EvalM (Maybe Expr)
    matchOnOnion x1 x2 = do
      me1 <- compatibility x1 x' pfm
      if isNothing me1 then compatibility x2 x' pfm else return me1
