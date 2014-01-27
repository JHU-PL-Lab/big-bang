{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

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
import Language.Haskell.TH (mkName)

import Language.TinyBang.Ast.Data
import Language.TinyBang.Ast.Origin
import Language.TinyBang.Utils.Display
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

instance Display IllFormedness where
  makeDoc ill = case ill of
    DuplicateDefinition x -> text "Duplicate definition:" <+> makeDoc x
    OpenExpression xs -> text "Expression is open in:" <+> makeDoc xs
    EmptyExpression o -> text "Empty expression at:" <+> makeDoc o
    EmptyPattern o -> text "Empty pattern at:" <+> makeDoc o

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
      , -- Check to see if any variables are free
        let free = findFreeVariables e in
        if Set.null free then Set.empty else Set.singleton $ OpenExpression free
      ]

-- |A routine to locate all variables bound by the provided expression.
countVariableBindings :: Expr -> VarCountMap
countVariableBindings = reduce CountVariableBindings

data CountVariableBindings = CountVariableBindings
newtype VarCountMap = VarCountMap (Map Var Int)
unVarCountMap :: VarCountMap -> Map Var Int
unVarCountMap (VarCountMap m) = m

-- |A routine to locate variables free in the provided expression.
findFreeVariables :: Expr -> Set Var
findFreeVariables e = fvdFree $ reduce FindFreeVariables e

data FindFreeVariables = FindFreeVariables
data FreeVarData = FreeVarData
  { fvdBound :: Set Var
  , fvdFree :: Set Var
  }

-- == Template Haskell jiggery-pokery begins here == ---------------------------

-- For countVariableBindings ---------------------------------------------------

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
  reduce CountVariableBindings (Clause _ x v) =
    VarCountMap (Map.singleton x 1) `mappend` reduce CountVariableBindings v

instance Reduce CountVariableBindings Redex (VarCountMap) where
  reduce CountVariableBindings redex =
    case redex of
      Def _ (VScape _ pattern expr) ->
        reduce CountVariableBindings pattern `mappend`
        reduce CountVariableBindings expr
      _ -> VarCountMap Map.empty

instance Reduce CountVariableBindings PatternClause VarCountMap where
  reduce CountVariableBindings (PatternClause _ x _) =
    VarCountMap (Map.singleton x 1)

-- For findFreeVariables -------------------------------------------------------

-- FIXME: this incorrectly assumes that anything bound within the pattern is
--        also bound outside of the pattern

instance Monoid FreeVarData where
  mempty = FreeVarData Set.empty Set.empty
  mappend fvd1 fvd2 =
    FreeVarData
      (fvdBound fvd1 `Set.union` fvdBound fvd2)
      (fvdFree fvd1 `Set.union` (fvdFree fvd2 Set.\\ fvdBound fvd1))

$(concat <$> mapM (defineCatInstance [t|FreeVarData|] ''FindFreeVariables)
    [ ''Expr
    , ''Redex
    , ''Pattern
    , ''PatternValue
    ]
 )
$(concat <$> mapM (defineReduceEmptyInstance [t|FreeVarData|] ''FindFreeVariables)
    [ ''Origin
    , ''LabelName
    , ''PrimitiveValue
    , ''PrimitiveType
    , ''BuiltinOp
    ]
 )
$(defineCommonCatInstances [t|FreeVarData|] ''FindFreeVariables)

instance Reduce FindFreeVariables Clause FreeVarData where
  reduce FindFreeVariables (Clause _ x r) =
    FreeVarData (Set.singleton x) Set.empty `mappend`
      reduce FindFreeVariables r

instance Reduce FindFreeVariables PatternClause FreeVarData where
  reduce FindFreeVariables pcl =
    case pcl of
      PatternClause _ x pv ->
        FreeVarData (Set.singleton x) Set.empty `mappend`
          reduce FindFreeVariables pv

instance Reduce FindFreeVariables Var FreeVarData where
  reduce FindFreeVariables x = FreeVarData Set.empty $ Set.singleton x

$(defineCatFunc [t|FreeVarData|] (mkName "ffvValue") ''FindFreeVariables ''Value) 

instance Reduce FindFreeVariables Value FreeVarData where
  reduce FindFreeVariables v =
    case v of
      VScape _ pat expr ->
        let inpat = reduce FindFreeVariables pat in
        let inexpr = reduce FindFreeVariables expr in
        -- But the bindings don't leave the scope of the scape
        FreeVarData Set.empty $ fvdFree $ inpat `mappend` inexpr
      _ -> ffvValue FindFreeVariables v
