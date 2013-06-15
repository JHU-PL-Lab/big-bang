{-# LANGUAGE TupleSections #-}

{-|
  A module containing the compatibility relations for evaluation.
-}

module Language.TinyBang.Interpreter.Compatibility
( applicationCompatibility
, compatibility
, CompatibilityArgument(..)
, Substitution(..)
) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM2)
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Interpreter.Projection
import Utils.Monad (ifM)

-- |A data type for compatibility arguments.
data CompatibilityArgument
  = ArgVal FlowVar
  | ArgExn FlowVar
  
-- |Calculates application compatibility for evaluation.  Unlike the
--  specification, this function assumes that the *first* value in the list is
--  the highest-priority scape.
applicationCompatibility :: CompatibilityArgument
                         -> [Value]
                         -> EvalM (Maybe Expr)
applicationCompatibility arg scapes =
  case scapes of
    VScape _ pat (Expr orig cls) : scapes' -> do
      mresult <- compatibility arg pat
      case mresult of
        Just (ss, Expr orig0 cls0) ->
          return $ Just $ substitute (collect $ Set.toList ss) $
            Expr (ComputedOrigin [orig,orig0]) $ cls0 ++ cls
        Nothing -> applicationCompatibility arg scapes'
    _ : scapes' -> applicationCompatibility arg scapes'
    _ -> return Nothing
        
-- |Calculates compatibility for evaluation.
compatibility :: CompatibilityArgument
              -> Pattern
              -> EvalM (Maybe (Set Substitution, Expr))
compatibility arg pat =
  case (arg, pat) of
    (ArgVal x1, ValuePattern orig y2 ipat) -> doMatch orig x1 y2 ipat
    (ArgExn x1, ExnPattern orig y2 ipat) -> doMatch orig x1 y2 ipat
    _ -> return Nothing
  where
    doMatch :: Origin -> FlowVar -> CellVar -> InnerPattern
            -> EvalM (Maybe (Set Substitution, Expr))
    doMatch orig x y ipat = do
      let orig' = ComputedOrigin [orig]
      msubsts <- innerCompatibility x ipat
      return $ (, Expr orig' [Evaluated $ CellDef orig' qualFinal y x])
        <$> msubsts
    
-- |Calculates compatibility for inner patterns.
innerCompatibility :: FlowVar
                   -> InnerPattern
                   -> EvalM (Maybe (Set Substitution))
innerCompatibility x1 ipat =
  case ipat of
    PrimitivePattern _ p ->
      ifM (isNothing <$> project x1 (anyProjPrim p))
        (return Nothing)
        (return $ Just Set.empty)
    LabelPattern _ n y2 ipat' -> do
      mv <- project x1 $ anyProjLabel n
      case mv of
        Just (VLabel _ n' y3) | n == n' -> do
          x4 <- cellLookup y3
          inner <- innerCompatibility x4 ipat'
          return $ Set.insert (CellSubstitution y3 y2) <$> inner
        Just v -> error $ "projectAll for label " ++ show n
                    ++ " produced a bad value: " ++ show v
        Nothing -> return Nothing
    ConjunctionPattern _ ipat1 ipat2 ->
      liftM2 Set.union <$> innerCompatibility x1 ipat1
                       <*> innerCompatibility x1 ipat2
    ScapePattern _ ->
      ifM (isNothing <$> project x1 anyProjFun)
        (return Nothing)
        (return $ Just Set.empty)
    EmptyOnionPattern _ -> return $ Just Set.empty
