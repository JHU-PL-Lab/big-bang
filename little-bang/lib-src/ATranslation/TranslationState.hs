{-|
  This module defines the state of the ATranslation process
-}

module ATranslation.TranslationState
( TranslationState (..)
, incrementFlowVarCount
, incrementCellVarCount
, insertVar)

where

import qualified Language.TinyBang.Ast.Data as TBA
import Data.Map

-- | ATranslation state: counter for variables and variable map                        
data TranslationState = TranslationState {
               flowVarCount :: Integer,
               cellVarCount :: Integer,
               varMap :: Map String TBA.CellVar
               }

-- | Methods that operate on TranslationState
incrementFlowVarCount :: TranslationState -> TranslationState
incrementFlowVarCount (TranslationState f c m) = TranslationState (f + 1) c m

incrementCellVarCount :: TranslationState -> TranslationState
incrementCellVarCount (TranslationState f c m) = TranslationState f (c + 1) m

insertVar :: String -> TBA.CellVar -> TranslationState -> TranslationState
insertVar varName cellVar (TranslationState f c m) = 
     TranslationState f c newMap
       where newMap = insert varName cellVar m
