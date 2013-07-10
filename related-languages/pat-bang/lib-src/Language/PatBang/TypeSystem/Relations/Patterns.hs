{-|
  This module contains basic operations over patterns.
-}
module Language.PatBang.TypeSystem.Relations.Patterns
( patternSubstitute
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Language.PatBang.TypeSystem.Types

-- |Performs pattern substitution on a given pattern variable.
patternSubstitute :: Map PatTVar PatternBody -> PatternBody -> PatternBody
patternSubstitute replacements body =
  case body of
    PPrim t -> PPrim t
    PLabel n tpat -> PLabel n $ rec tpat
    PFun -> PFun
    PPat -> PPat
    PScape -> PScape
    PConj tpat tpat' -> PConj (rec tpat) (rec tpat')
    PDisj tpat tpat' -> PDisj (rec tpat) (rec tpat')
    PSubst a tpats -> PSubst a $ map rec tpats
    PRec b' tpat ->
      PRec b' $ patternSubstitute (Map.delete b' replacements) tpat
    PPatternOf a -> PPatternOf a
    PVar b' -> fromMaybe (PVar b') (Map.lookup b' replacements)
    PNone -> PNone
  where
    rec :: PatternBody -> PatternBody
    rec = patternSubstitute replacements