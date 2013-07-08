{-# LANGUAGE TupleSections #-}

{-|
  A module containing the compatibility relations for evaluation.
-}

module Language.PatBang.Interpreter.Compatibility
( applicationCompatibility
, compatibility
, Substitution(..)
) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM2)
import Data.Maybe (isNothing)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast
import Language.PatBang.Interpreter.Basis
import Language.PatBang.Interpreter.Projection

type Bindings = Map FlowVar Value

-- |Calculates application compatibility for evaluation.  Unlike the
--  specification, this function assumes that the *first* value in the list is
--  the highest-priority scape.
applicationCompatibility :: FlowVar -> [Value] -> EvalM (Maybe Expr)
applicationCompatibility x1 scapes =
  case scapes of
    VScape _ x2 x3 : scapes' -> do
    --VScape _ pat (Expr orig cls) : scapes' -> do
      pats <- projectAll x2 anyProjPat
      funs <- projectAll x3 anyProjFun
      if null pats || null funs
        then return Nothing
        else case (head pats, head funs) of
          (   VPattern _ (AstList _ pvars) pat
            , VFunction _ (AstList _ vvars) (Expr _ cls)) ->
            do
              mvalues <- compatibilityTop x1 (pvars, pat)
              case mvalues of
                Nothing -> return Nothing
                Just values ->
                  if length values < length vvars
                    then applicationCompatibility x1 scapes'
                    else do
                      let cls' = zipWith (ValueDef generated) vvars values
                      return $ Just $ Expr generated $ cls' ++ cls
          _ -> error "Projection gave inconsistent results"
    _ : scapes' -> applicationCompatibility x1 scapes'
    _ -> return Nothing
    
compatibilityTop :: FlowVar -> ([PatVar], PatternBody) -> EvalM (Maybe [Value])
compatibilityTop = undefined -- TODO

compatibility :: FlowVar -> PatternBody -> Set PatternBody
              -> EvalM (Maybe Bindings)
compatibility = undefined -- TODO
