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
  PSubst orig x (AstList orig' pats) ->
    PSubst orig x $ AstList orig' $ map rec pats
  PRec orig b pat' ->
    PRec orig b $ patternSubstitute (Map.delete b replacements) pat'
  PVar orig b -> fromMaybe (PVar orig b) (Map.lookup b replacements)
  where
    rec :: PatternBody -> PatternBody
    rec = patternSubstitute replacements
