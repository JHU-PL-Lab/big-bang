module Language.TinyBang.Interpreter.Projection
( project
, projectAll
) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Trans.Either

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis
              
-- |Performs single evaluation projection on a given variable and projector.
project :: FlowVar -> Projector -> EvalM Value
project x proj = do
  vs <- projectAll x proj
  if null vs then left $ ProjectionFailure x proj else return $ last vs

-- |Performs evaluation projection on a given variable and projector.
projectAll :: FlowVar -> Projector -> EvalM [Value]
projectAll x proj = do
  v <- flowLookup x
  case v of
    VOnion _ x' x'' -> (++) <$> projectAll x' proj <*> projectAll x'' proj
    VOnionFilter _ x' op proj' ->
      let fcond = case op of
                    OpOnionSub _ -> (== proj')  
                    OpOnionProj _ -> (/= proj')
      in if fcond proj then return [] else projectAll x' proj
    _ -> return $ if inProjector v proj then [v] else []
  where
    inProjector :: Value -> Projector -> Bool
    inProjector v proj' =
      case (v, proj') of
        (VInt _ _, ProjPrim _ (PrimInt _)) -> True
        (VChar _ _, ProjPrim _ (PrimChar _)) -> True
        (VLabel _ n _, ProjLabel _ n') | n == n' -> True
        (VScape _ _ _, ProjFun _) -> True
        _ -> False
