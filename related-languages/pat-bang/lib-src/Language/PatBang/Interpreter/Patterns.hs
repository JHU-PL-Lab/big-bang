module Language.PatBang.Interpreter.Patterns
( patternSubstitute
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Language.PatBang.Ast

-- |Performs substitution in a pattern.
patternSubstitute :: Map PatVar PatternBody -> PatternBody -> PatternBody
patternSubstitute replacements pat = case pat of
  PPrim orig t -> PPrim orig t
  PLabel orig n pat' -> PLabel orig n $ rec pat'
  PFun orig -> PFun orig
  PPat orig -> PPat orig
  PScape orig -> PScape orig
  PConj orig pat' pat'' -> PConj orig (rec pat') (rec pat'')
  PDisj orig pat' pat'' -> PDisj orig (rec pat') (rec pat'')
  PSubst orig x (AstList orig' pats) ->
    PSubst orig x $ AstList orig' $ map rec pats
  PRec orig y pat' ->
    PRec orig y $ patternSubstitute (Map.delete y replacements) pat'
  PPatternOf orig x -> PPatternOf orig x
  PVar orig y -> fromMaybe (PVar orig y) (Map.lookup y replacements)
  PNone orig -> PNone orig
  where
    rec :: PatternBody -> PatternBody
    rec = patternSubstitute replacements
