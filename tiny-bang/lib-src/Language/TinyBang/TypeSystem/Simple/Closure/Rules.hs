{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, TemplateHaskell #-}
module Language.TinyBang.TypeSystem.Simple.Closure.Rules
( computeClosure
) where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.NonDet
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Simple.Closure.Basis
import Language.TinyBang.TypeSystem.Simple.Closure.Builtins
import Language.TinyBang.TypeSystem.Simple.Data
import Language.TinyBang.TypeSystem.Simple.Matching
import Language.TinyBang.TypeSystem.Simple.Polymorphism
import Language.TinyBang.TypeSystem.Simple.Variables
import Language.TinyBang.Utils.Display as D
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

computeClosure :: ConstraintSet -> (Set Inconsistency, ConstraintSet)
computeClosure cs =
  _debugI (display $ text "Performing constraint closure on:" </>
                     indent 2 (makeDoc cs)) $ 
  closeToFixedPoint (mempty, cs) closureStep
  where
    closeToFixedPoint :: (Eq a) => a -> (a -> a) -> a
    closeToFixedPoint x f =
      let out = f x in
      if x == out then out else closeToFixedPoint out f
    closureStep :: (Set Inconsistency, ConstraintSet)
                -> (Set Inconsistency, ConstraintSet)
    closureStep (incons1, cs1) =
      postLog _debugI (
        \(incons,constraints) ->
          display $ text "Constraint closure step gives constraints:" </>
                    indent 2 (makeDoc constraints) </>
                    if Set.null incons then D.empty else
                      text "with inconsistencies:" </>
                      indent 2 (makeDoc incons)
      ) $
      let (incons2, cs2) = singleClosurePass cs1 in
      (incons1 `mappend` incons2, cs1 `mappend` cs2)
  
singleClosurePass :: ConstraintSet -> (Set Inconsistency, ConstraintSet)
singleClosurePass cs =
  mconcat $ map runClosureRule
    [ transitivityRule
    , applicationRule
    , builtinRule
    ]
  where
    runClosureRule :: ClosureM ConstraintSet
                   -> (Set Inconsistency, ConstraintSet)
    runClosureRule rule = second mconcat $ runClosureM rule cs
  
transitivityRule :: ClosureM ConstraintSet
transitivityRule = do
  cs <- Set.toList <$> unConstraintSet <$> ask
  LowerBoundConstraint ft a1 <- choose cs
  IntermediateConstraint a1' a2 <- choose cs
  guard $ a1 == a1'
  return $ ConstraintSet $ Set.singleton $ ft <: a2

applicationRule :: ClosureM ConstraintSet
applicationRule = do
  cs <- Set.toList <$> unConstraintSet <$> ask
  ApplicationConstraint a0 a1 a2 <- choose cs
  matches <- findApplicationMatches a0 a1 <$> ask
  match <- choose matches
  case match of
    SuccessfulMatchResult
      { mrBodyRoot = a1'
      , mrBodyConstraints = cs1'
      , mrBindings = cs1''
      } ->
      let sigma = polyinstFnForConstraintSet (cs1' `mappend` cs1'') a2 in
      let (a2',cs2') = substituteVars sigma (a1',cs1') in
      let cs2'' = tFresh sigma cs1'' in
      return $ mconcat [cs2', cs2'', ConstraintSet $ Set.singleton $ a2' <: a2]
    FailedMatchResult ->
      -- TODO: put info in the inconsistency type
      reportInconsistency TODO_ReplaceMe_Inconsistency

builtinRule :: ClosureM ConstraintSet
builtinRule = do
  cs <- Set.toList <$> unConstraintSet <$> ask
  BuiltinOpConstraint op as a <- choose cs
  (cs',t) <- builtinTypeEval op as a
  return $ mappend cs' $
    ConstraintSet $ Set.singleton $ FilteredType t mempty mempty <: a
