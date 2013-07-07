{-# LANGUAGE TupleSections, ScopedTypeVariables, TemplateHaskell #-} 

module Language.TinyBang.TypeSystem.Relations.Compatibility
( CompatibilityArgument(..)
, CompatM
, checkApplicationCompatible
, checkCompatible
, CellSubstitutions
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Data.Map (Map)
import qualified Data.Map as Map

import Language.TinyBang.Ast as A
import Language.TinyBang.Display
import Language.TinyBang.Logging
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.ConstraintDatabase
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Data.Compatibility
import Language.TinyBang.TypeSystem.Fibrations
import Language.TinyBang.TypeSystem.Monad.Trans.Flow
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Relations.Projection
import Language.TinyBang.TypeSystem.Types as T

$(loggingFunctions)

{- TODO: for performance, the compatibility and projection relations should
         accept a fibration as input; this fibration should constrain the
         choices those relations make.  The problem at the moment is that the
         current implementation fans out a number of paths which will wind up
         being discarded at a later mergeFibrations call.
-}

type CellSubstitutions = Map CellTVar CellTVar

type CompatM db m = FlowT (EitherT (ProjectionError db) m)

checkApplicationCompatible :: forall db m.
                              ( Applicative m, ConstraintDatabase db
                              , MonadCReader db m, Display db)
                           => CompatibilityArgument
                           -> [Type db]
                           -> CompatM db m (Maybe (FlowTVar, db), Fibration db)
checkApplicationCompatible arg ts =
  _debugI ("Checking application compatibility for " ++ display arg ++ " with "
              ++ display ts) $
  do
    (result,fib) <- doCheck Unexpanded ts
    _debug $ "Application compatibility for " ++ display arg ++ " with "
        ++ display ts ++ " at fibration " ++ display fib ++
          case result of
            Nothing -> " can fail"
            Just (a,cs) -> " can succeed with " ++ display a ++ "\\"
                              ++ display cs
    return (result,fib)
  where
    doCheck :: Fibration db
            -> [Type db]
            -> CompatM db m (Maybe (FlowTVar, db), Fibration db)
    doCheck fib scapes =
      case scapes of
        [] ->
          _debugI ("Application compatibility check for argument "
                      ++ display arg ++ " exhausted with fibration "
                      ++ display fib) $
          return (Nothing, fib)
        scape@(Scape tpat a cs) : scapes' -> do
          _debug $ "Checking application compatibility for " ++ display arg
                      ++ " with " ++ display scape 
          (dat,fib') <- checkCompatible arg tpat
          case mergeFibrations fib fib' of
            Nothing -> mzero
            Just fib'' ->
              let debugPfx = "Application compatibility for " ++ display arg
                    ++ " with " ++ display scape ++ " at fibration "
                    ++ display fib'' in
              case dat of
                Nothing ->
                  _debugI (debugPfx ++ " can fail; moving on...") $
                  doCheck fib'' scapes'
                Just (substs, cs0) ->
                  let csOut = substituteCellVariables substs cs `union` cs0 in
                  _debugI (debugPfx ++ " succeeds")
                  return (Just (a, csOut), fib'')
        t:_ -> error $ "Called checkApplicationCompatible with list "
                    ++ "containing non-scape type " ++ display t

checkCompatible :: forall db m.
                   ( Applicative m, ConstraintDatabase db, MonadCReader db m
                   , Display db)
                => CompatibilityArgument
                -> PatternType
                -> CompatM db m (Maybe (CellSubstitutions, db), Fibration db)
checkCompatible arg tpat = do
  _debug $ "Checking compatibility of " ++ display arg ++ " with "
              ++ display tpat
  (mdat, fib) <- 
    case (arg,tpat) of
      (ArgVal a, T.ValuePattern b tipat) -> patternMatch a b tipat
      (ArgExn a, T.ExnPattern b tipat) -> patternMatch a b tipat
      (ArgVal _, T.ExnPattern _ _) -> failure
      (ArgExn _, T.ValuePattern _ _) -> failure
  _debug $ "Compatibility of " ++ display arg ++ " with " ++ display tpat
              ++ " at fibration " ++ display fib
              ++ case mdat of
                    Nothing -> " can fail"
                    Just (substs, cs) -> " can succeed with substitutions "
                        ++ display substs ++ " and constraints " ++ display cs
  return (mdat, fib)
  where
    failure = return (Nothing, Unexpanded)
    patternMatch :: FlowTVar -> CellTVar -> InnerPatternType
                 -> CompatM db m (Maybe (CellSubstitutions, db), Fibration db)
    patternMatch a b tipat = do
      (msubsts, fib) <- checkInnerCompatible a tipat
      case msubsts of
        Nothing -> return (Nothing, fib)
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
                        , MonadCReader db m, Display db)
                     => FlowTVar
                     -> InnerPatternType
                     -> CompatM db m (Maybe CellSubstitutions, Fibration db)
checkInnerCompatible a1 tipat =
  case tipat of
    T.PrimitivePattern p -> do
      (x,fib) <- liftCompat $ project (projPrim p) a1
      return ((if x then Just Map.empty else Nothing), fib)
    T.LabelPattern n b2 tipat' -> do
      (bs,fibfn) <- liftCompat $ project (projLabel n) a1
      if null bs
        then return (Nothing, fibfn [])
        else do
          let b3 = head bs
          lbc <- flow $ lift $ getCellLowerBoundConstraints b3 <$> askDb
          let a4 = lowerBoundOf lbc
          (msubsts, fib1') <- checkInnerCompatible a4 tipat'
          return (Map.insert b3 b2 <$> msubsts, fibfn [fib1'])
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
    T.ScapePattern -> do
      (ts,fib) <- liftCompat $ project projFun a1
      return ((if null ts then Nothing else Just Map.empty), fib)
    T.EmptyOnionPattern -> return (Just Map.empty, Unexpanded)

liftCompat :: ProjM db m a -> CompatM db m a
liftCompat = id
