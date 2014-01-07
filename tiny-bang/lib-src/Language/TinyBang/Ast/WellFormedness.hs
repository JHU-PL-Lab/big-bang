{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, FlexibleInstances #-}

module Language.TinyBang.Ast.WellFormedness
( checkWellFormed
, IllFormedness(..)
) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid

import Language.TinyBang.Ast.Data
import Language.TinyBang.Ast.Origin
import Language.TinyBang.Utils.TemplateHaskell.Reduce

-- |A data structure representing types of ill-formed expressions.
data IllFormedness
  = DuplicateDefinition Var
      -- ^Generated when a variable is declared twice.
  | OpenExpression (Set Var)
      -- ^Generated when the expression is not closed.
  | EmptyExpression Origin
      -- ^Generated when an empty expression (an expression with no clauses) is
      --  encountered
  | EmptyPattern Origin
      -- ^Generated when an empty pattern (a pattern with no clauses) is
      --  encountered.
  deriving (Eq, Ord, Show)

-- TODO: check to ensure that the expression is closed

checkWellFormed :: Expr -> Set IllFormedness
checkWellFormed e =
  Set.unions checks
  where
    checks :: [Set IllFormedness]
    checks =
      [
        -- Check to see if any variables are defined as duplicates
        Set.map DuplicateDefinition $
          Set.fromList (map fst $ filter ((> 1) . snd) $
            Map.toList $ unVarCountMap $ countVariableBindings e)
      ]

-- |A routine to locate all variables bound by the provided expression.
countVariableBindings :: Expr -> VarCountMap
countVariableBindings = reduce CountVariableBindings

data CountVariableBindings = CountVariableBindings
newtype VarCountMap = VarCountMap (Map Var Int)
unVarCountMap :: VarCountMap -> Map Var Int
unVarCountMap (VarCountMap m) = m
instance Monoid VarCountMap where
  mempty = VarCountMap Map.empty
  mappend (VarCountMap m1) (VarCountMap m2) =
    VarCountMap $ Map.unionWith (+) m1 m2

$(concat <$> mapM (defineCatInstance [t|VarCountMap|] ''CountVariableBindings)
                  [ ''Expr
                  , ''Pattern
                  ])
$(concat <$> mapM (defineReduceEmptyInstance [t|VarCountMap|] ''CountVariableBindings)
                  [ ''Origin
                  ])
$(defineCommonCatInstances [t|VarCountMap|] ''CountVariableBindings)

instance Reduce CountVariableBindings Clause (VarCountMap) where
  reduce CountVariableBindings cl =
    case cl of
      RedexDef _ x _ -> VarCountMap $ Map.singleton x 1
      Evaluated (ValueDef _ x _) -> VarCountMap $ Map.singleton x 1
