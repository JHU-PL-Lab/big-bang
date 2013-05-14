{-# LANGUAGE TupleSections, ScopedTypeVariables #-} 

module Language.TinyBang.TypeSystem.Relations.Compatibility
( CompatibilityArgument(..)
, CompatM
, checkApplicationCompatible
, checkCompatible
, CellSubstitutions
) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast as A
import Language.TinyBang.Display
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Monad.Trans.Flow
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Relations.Projection
import Language.TinyBang.TypeSystem.Types as T

-- |A data structure representing the argument type passed to a pattern
--  compatibility check.
data CompatibilityArgument
  = ArgVal FlowTVar
  | ArgExn FlowTVar

type CellSubstitutions = Map CellTVar CellTVar

type CompatM db m = FlowT (EitherT (ProjectionError db) m)

checkApplicationCompatible :: forall db m.
                              ( Applicative m, ConstraintDatabase db
                              , MonadCReader db m, Show db)
                           => CompatibilityArgument
                           -> [Type db]
                           -> CompatM db m (Maybe (FlowTVar, db), Fibration db)
checkApplicationCompatible arg = doCheck Unexpanded
  where
    doCheck :: Fibration db
            -> [Type db]
            -> CompatM db m (Maybe (FlowTVar, db), Fibration db)
    doCheck fib scapes =
      case scapes of
        [] -> return (Nothing, fib)
        (Scape tpat a cs):scapes' -> do
          (dat,fib') <- checkCompatible arg tpat
          case mergeFibrations fib fib' of
            Nothing -> mzero
            Just fib'' -> case dat of
              Nothing -> doCheck fib'' scapes'
              Just (substs, cs0) ->
                 let csOut = substituteCellVariables substs cs `union` cs0 in
                 return (Just (a, csOut), fib'')
        t:_ -> error $ "Called checkApplicationCompatible with list "
                    ++ "containing non-scape type " ++ show t -- TODO: display instead of show

checkCompatible :: forall db m.
                   (Applicative m, ConstraintDatabase db, MonadCReader db m)
                => CompatibilityArgument
                -> PatternType
                -> CompatM db m (Maybe (CellSubstitutions, db), Fibration db)
checkCompatible arg tpat =
  case (arg,tpat) of
    (ArgVal a, T.ValuePattern b tipat) -> patternMatch a b tipat
    (ArgExn a, T.ExnPattern b tipat) -> patternMatch a b tipat
    (ArgVal _, T.ExnPattern _ _) -> failure
    (ArgExn _, T.ValuePattern _ _) -> failure
  where
    failure = return (Nothing, Unexpanded)
    patternMatch :: FlowTVar -> CellTVar -> InnerPatternType
                 -> CompatM db m (Maybe (CellSubstitutions, db), Fibration db)
    patternMatch a b tipat = do
      (msubsts, fib) <- checkInnerCompatible a tipat
      case msubsts of
        Nothing -> failure
        Just substs ->
          let database = fromList
                   [ ( WrapCellCreationConstraint $ CellCreationConstraint a b
                     , CompatibilityWiring )
                   , ( WrapFinalConstraint $ FinalConstraint b
                     , CompatibilityWiring ) ]
          in return (Just (substs, database), fib)

-- |Determines whether or not a given variable and inner pattern type are
--  compatible.  If so, a set of cell substitutions and a fibration are
--  produced; otherwise, a @Nothing@ and a fibration are produced.
checkInnerCompatible :: ( Applicative m, ConstraintDatabase db
                        , MonadCReader db m)
                     => FlowTVar
                     -> InnerPatternType
                     -> CompatM db m (Maybe CellSubstitutions, Fibration db)
checkInnerCompatible a1 tipat =
  case tipat of
    T.PrimitivePattern p -> first emptyIfNotNull <$>
                                liftCompat (project (projPrim p) a1)
    T.LabelPattern n b2 tipat' -> do
      -- Assumption: label projection only yields label types and fibrations
      -- with exactly one child fibration.
      (typs,fib@(Fibration _ [fib1])) <- liftCompat $ project (projLabel n) a1
      if null typs
        then return (Nothing, fib)
        else do
          let (Label _ b3) = head typs
          a4 <- flow $ lift $ getCellOrStoreBounds b3 <$> askDb
          (msubsts, fib1') <- checkInnerCompatible a4 tipat'
          case mergeFibrations fib1 fib1' of
            Nothing -> mzero
            Just fib' ->
              return (Map.insert b3 b2 <$> msubsts, fib')
    T.ConjunctivePattern tipat1 tipat2 -> do
      (msubsts1, fib1) <- checkInnerCompatible a1 tipat1
      (msubsts2, fib2) <- checkInnerCompatible a1 tipat2
      case mergeFibrations fib1 fib2 of
        Nothing -> mzero
        Just fib ->
          let msubsts = do
                substs1 <- msubsts1
                substs2 <- msubsts2
                return $ Map.union substs1 substs2
          in return (msubsts,fib)
    T.ScapePattern -> first emptyIfNotNull <$> liftCompat (project projFun a1)
    T.EmptyOnionPattern -> return (Just Map.empty, Unexpanded)
  where
    emptyIfNotNull :: [a] -> Maybe (Map k b)
    emptyIfNotNull lst = if null lst then Nothing else Just Map.empty

liftCompat :: ProjM db m a -> CompatM db m a
liftCompat = id
