{-# LANGUAGE GADTs #-}

module Language.PatBang.Interpreter.Projection
( project
, projectAll
) where

import Control.Applicative ((<$>),(<*>))
import Data.Maybe (listToMaybe)

import Language.PatBang.Ast
import Language.PatBang.Interpreter.Basis
              
-- |Performs single evaluation projection on a given variable and projector.
project :: FlowVar -> AnyProjector -> EvalM (Maybe Value)
project x proj = listToMaybe <$> projectAll x proj

-- |Performs evaluation projection on a given variable and projector.  This
--  projection calculation produces a list in *reverse* order; the first element
--  is the highest priority value.
projectAll :: FlowVar -> AnyProjector -> EvalM [Value]
projectAll x proj = do
  v <- flowLookup x
  case v of
    VOnion _ x' x'' -> (++) <$> projectAll x'' proj <*> projectAll x' proj
    _ -> return $ if inProjector v proj then [v] else []
  where
    inProjector :: Value -> AnyProjector -> Bool
    inProjector v proj' =
      case (v, proj') of
        (VInt _ _, SomeProjector (ProjPrim _ (PrimInt _))) -> True
        (VLabel _ n _, SomeProjector (ProjLabel _ n')) | n == n' -> True
        (VFunction _ _ _, SomeProjector (ProjFun _)) -> True
        (VPattern _ _ _, SomeProjector (ProjPat _)) -> True
        (VScape _ _ _, SomeProjector (ProjScape _)) -> True
        _ -> False
