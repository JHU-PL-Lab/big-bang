{-# LANGUAGE TupleSections, TemplateHaskell #-}

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
import Language.PatBang.Display
import Language.PatBang.Interpreter.Basis
import Language.PatBang.Interpreter.Patterns
import Language.PatBang.Interpreter.Projection
import Language.PatBang.Logging

type Bindings = Map PatVar Value
type PossibleBindings = Maybe Bindings
type Visits = Set PatternBody

$(loggingFunctions)

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
                Nothing -> applicationCompatibility x1 scapes'
                Just values ->
                  if length values < length vvars
                    then applicationCompatibility x1 scapes'
                    else do
                      let cls' = zipWith (ValueDef generated) vvars values
                      return $ Just $ Expr generated $ cls' ++ cls
          _ -> error "Projection gave inconsistent results"
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

compatibility :: FlowVar -> PatternBody -> Visits
              -> EvalM PossibleBindings
compatibility x pat visits =
  _debugI ("Checking compatibility for " ++ display x ++ " with "
              ++ display pat) $
  _debugValI ("Compatibility resul for " ++ display x ++ " with "
              ++ display pat ++ " is: ") <$>
  case pat of
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
    PDisj _ pat' pat'' ->
      -- Try pat'' and, if that doesn't work, go with pat'
      fromMaybe <$> compatibility x pat' visits <*>
                      (Just <$> compatibility x pat'' visits)
    PSubst _ x' (AstList _ pats) -> do
      pattern <- project x' $ SomeProjector projPat
      case pattern of
        Just (VPattern _ (AstList _ ys) pat')
          | pat' `Set.member` visits -> return Nothing
          | length ys /= length pats ->
              evalError $ MismatchedPatternSubstitution ys pats
          | otherwise ->
              let pat'' = patternSubstitute (Map.fromList $ zip ys pats) pat' in
              compatibility x pat'' $ Set.insert pat' visits
        Just _ -> error "Projection produced non-pattern for pattern projector"
        Nothing -> return Nothing
    PRec _ y pat' ->
      if pat `Set.member` visits
        then return Nothing
        else
          let pat'' = patternSubstitute (Map.singleton y pat) pat' in
          compatibility x pat'' $ Set.insert pat visits
    PPatternOf _ x' -> do
      scapes <- projectAll x' $ SomeProjector projScape
      pat' <- foldl (PDisj generated) (PNone generated) <$>
                    catMaybes <$> mapM patternFromScape scapes
      -- Check compatibility with this pattern but toss any of its bindings
      fmap (const Map.empty) <$> compatibility x pat' visits
      where
        patternFromScape :: Value -> EvalM (Maybe PatternBody)
        patternFromScape v = case v of
          VScape _ xp _ -> do
            v' <- project xp $ SomeProjector projPat
            case v' of
              Just (VPattern _ _ pat'') -> return $ Just pat''
              Just _ -> error $ "Pattern projected produced non-pattern value "
                                  ++ display v'
              Nothing -> return Nothing
          _ -> error $ "Scape projection produced non-scape value " ++ display v
    PVar _ y -> Just . Map.singleton y <$> flowLookup x
    PNone _ -> return Nothing
    where
      primCompat :: Projector tag -> EvalM PossibleBindings
      primCompat proj = do
        mv <- project x $ SomeProjector proj
        return $ if isNothing mv then Nothing else Just Map.empty
