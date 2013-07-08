{-# LANGUAGE TupleSections #-}

{-|
  A module containing the compatibility relations for evaluation.
-}

module Language.PatBang.Interpreter.Compatibility
( applicationCompatibility
, compatibility
, Substitution(..)
) where

import Control.Applicative
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast
import Language.PatBang.Interpreter.Basis
import Language.PatBang.Interpreter.Patterns
import Language.PatBang.Interpreter.Projection

type Bindings = Map PatVar Value
type PossibleBindings = Maybe Bindings

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
compatibilityTop x (ys, pat) = do
  pbinds <- compatibility x pat Set.empty
  case pbinds of
    Nothing -> return Nothing
    Just binds -> return $ Just $ gather ys binds
  where
    gather :: [PatVar] -> Bindings -> [Value]
    gather ys' binds = case ys' of
      [] -> []
      y:ys'' -> case Map.lookup y binds of
                  Nothing -> []
                  Just v -> v : gather ys'' binds

compatibility :: FlowVar -> PatternBody -> Set PatternBody
              -> EvalM PossibleBindings
compatibility x pat visits = case pat of
  PPrim _ p -> primCompat $ projPrim p
  PLabel _ n pat' -> do
    val <- project x $ SomeProjector $ projLabel n
    case val of
      Just (VLabel _ _ x') -> compatibility x' pat' Set.empty
      Just _ -> error "Projection of label produced non-label value!"
      Nothing -> return Nothing
  PFun _ -> primCompat projFun
  PPat _ -> primCompat projPat
  PScape _ -> primCompat projScape
  PConj _ pat' pat'' -> do
    mb1 <- compatibility x pat' visits
    mb2 <- compatibility x pat'' visits
    case (mb1, mb2) of
      (Just binds1, Just binds2) -> return $ Just $ Map.union binds2 binds1
      (_, _) -> return Nothing
  PSubst _ x' (AstList _ pats) -> checkVisits $ do
    pattern <- project x' $ SomeProjector projPat
    case pattern of
      Just (VPattern _ (AstList _ ys) pat') ->
        if length ys /= length pats
          then evalError $ MismatchedPatternSubstitution ys pats
          else
            let pat'' = patternSubstitute (Map.fromList $ zip ys pats) pat' in
            compatibility x pat'' $ Set.insert pat visits
      Just _ -> error "Projection produced non-pattern for pattern projector"
      Nothing -> return Nothing
  PRec _ y pat' -> checkVisits $
    let pat'' = patternSubstitute (Map.singleton y pat) pat' in
    compatibility x pat'' $ Set.insert pat visits
  PVar _ y -> Just . Map.singleton y <$> flowLookup x
  where
    primCompat :: Projector tag -> EvalM PossibleBindings
    primCompat proj = do
      mv <- project x $ SomeProjector proj
      return $ if isNothing mv then Nothing else Just Map.empty
    checkVisits :: EvalM PossibleBindings -> EvalM PossibleBindings
    checkVisits ans = if pat `Set.member` visits
                        then return $ Just Map.empty else ans
