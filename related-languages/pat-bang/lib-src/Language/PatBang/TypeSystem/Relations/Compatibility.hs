{-# LANGUAGE TupleSections, ScopedTypeVariables, TemplateHaskell #-} 

module Language.PatBang.TypeSystem.Relations.Compatibility
( CompatM
, checkApplicationCompatible
, checkTopCompatible
, checkCompatible
, Bindings
, PossibleBindings
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.PatBang.Ast as A
import Language.PatBang.Display
import Language.PatBang.Logging
import Language.PatBang.TypeSystem.Constraints
import Language.PatBang.TypeSystem.ConstraintDatabase
import Language.PatBang.TypeSystem.ConstraintHistory
import Language.PatBang.TypeSystem.Data.Projection
import Language.PatBang.TypeSystem.Fibrations
import Language.PatBang.TypeSystem.Monad.Trans.Flow
import Language.PatBang.TypeSystem.Monad.Trans.CReader
import Language.PatBang.TypeSystem.Relations.Patterns
import Language.PatBang.TypeSystem.Relations.Projection
import Language.PatBang.TypeSystem.Types as T

$(loggingFunctions)

type CompatM db m = FlowT (EitherT (ProjectionError db) m)
type Bindings db = Map PatTVar (Type db)
type PossibleBindings db = Maybe (Bindings db)
type Visits = Set T.PatternBody

checkApplicationCompatible :: forall db m.
                              ( Applicative m, ConstraintDatabase db
                              , MonadCReader db m, Display db)
                           => FlowTVar
                           -> [Type db]
                           -> CompatM db m (Maybe (FlowTVar, db), Fibration db)
checkApplicationCompatible a1 ts =
  _debugI ("Checking application compatibility for " ++ display a1 ++ " with "
              ++ display ts) $
  do
    (result,fib) <- doCheck Unexpanded ts
    _debug $ "Application compatibility for " ++ display a1 ++ " with "
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
                      ++ display a1 ++ " exhausted with fibration "
                      ++ display fib) $
          return (Nothing, fib)
        scape@(Scape a2 a3) : scapes' -> do
          _debug $ "Checking application compatibility for " ++ display a1
                      ++ " with " ++ display scape
          -- Get the individual pieces of the scape
          (patParts,_) <- liftCompat $ project projPat a2
          (funParts,_) <- liftCompat $ project projFun a3
          if null funParts || null patParts
            then return (Nothing, fib)
            else do
              let (pvars,tpat):_ = patParts
              let (tvars,rtvar,db):_ = funParts
              -- See if the argument matches
              (mtyps, fib') <- checkTopCompatible a1 (pvars,tpat)
              case mergeFibrations fib fib' of
                Nothing -> mzero
                Just fib'' ->
                  let debugPfx = "Application compatibility for " ++ display a1
                        ++ " with " ++ display scape ++ " at fibration "
                        ++ display fib'' in
                  case mtyps of
                    Nothing ->
                      _debugI (debugPfx ++ " can fail; moving on...") $
                      doCheck fib'' scapes'
                    Just typs ->
                      if length typs < length tvars
                        then _debugI (debugPfx ++
                          " does not bind enough arguments; moving on...") $
                          doCheck fib'' scapes'
                        else do
                          let histList = CompatibilityWiring : histList
                          let cs = map cwrap $ zipWith TypeConstraint typs tvars
                          let newdb = db `union` fromList (zip cs histList)
                          return (Just (rtvar, newdb), fib'')
        t:_ -> error $ "Called checkApplicationCompatible with list "
                    ++ "containing non-scape type " ++ display t

-- |Performs a compatibility check 
checkTopCompatible :: forall db m.
                      ( Applicative m, ConstraintDatabase db, MonadCReader db m
                      , Display db)
                   => FlowTVar
                   -> ([PatTVar], T.PatternBody)
                   -> CompatM db m (Maybe [Type db], Fibration db)
checkTopCompatible a1 (pvars, tpat) = do
  (pbinds, fib) <- checkCompatible a1 tpat Set.empty
  case pbinds of
    Nothing -> return (Nothing, fib)
    Just binds -> return (Just $ gather pvars binds, fib)
  where
    gather :: [PatTVar] -> Map PatTVar (Type db) -> [Type db]
    gather pvs m = case pvs of
      [] -> []
      pv:pvs' -> case Map.lookup pv m of
                    Nothing -> []
                    Just t -> t : gather pvs' m

checkCompatible :: forall db m.
                   ( Applicative m, ConstraintDatabase db, MonadCReader db m
                   , Display db)
                => FlowTVar -> T.PatternBody -> Visits
                -> CompatM db m (PossibleBindings db, Fibration db)
checkCompatible a1 tpat visits = case tpat of
  T.PPrim p -> primCompat (projPrim p) id
  T.PLabel n tpat' -> do
    (contents, fibfn) <- liftCompat $ project (projLabel n) a1
    if null contents
      then return (Nothing, fibfn [])
      else do
        (pbs, fib) <- checkCompatible (head contents) tpat' visits
        return (pbs, fibfn [fib])
  T.PFun -> primCompat projFun $ first ((> 0) . length)
  T.PPat -> primCompat projPat $ first ((> 0) . length)
  T.PScape -> primCompat projScape $
                ((> 0).  length) *** ($ zip unexpandeds unexpandeds)
  T.PConj tpat1 tpat2 -> do
    (pbs1, fib1) <- checkCompatible a1 tpat1 visits
    (pbs2, fib2) <- checkCompatible a1 tpat2 visits
    Just fib' <- return $ mergeFibrations fib1 fib2
    case (pbs1, pbs2) of
      (Just bs1,Just bs2) -> return (Just $ Map.union bs2 bs1, fib')
      (_, _) -> return (Nothing, fib')
  T.PSubst a2 tpatsArg -> checkVisits $ do
    (tpatsAppl, _) <- liftCompat $ project projPat a2
    if null tpatsAppl
      then return (Nothing, Unexpanded)
      else do
        let (pvars, tpat') = head tpatsAppl
        if length pvars /= length tpatsArg
          then return (Nothing, Unexpanded)
          else
            let tpat'' =
                  patternSubstitute (Map.fromList $ zip pvars tpatsArg) tpat' in
            checkCompatible a1 tpat'' $ tpat `Set.insert` visits
  T.PRec b tpat' -> checkVisits $
    let tpat'' = patternSubstitute (Map.singleton b tpat) tpat' in
    checkCompatible a1 tpat'' $ tpat `Set.insert` visits
  T.PVar b -> do
    TypeConstraint typ _ <- flow $ getTypeConstraintsByUpperBound a1 <$> askDb
    return (Just $ Map.singleton b typ, blankFibrationFor typ)
  where
    -- |Takes a projector and a conversion function.  The conversion function
    --  maps the result of projection onto a pair of boolean and fibration.
    primCompat :: Projector tag
               -> (MultiProjection db tag -> (Bool, Fibration db)) 
               -> CompatM db m (PossibleBindings db, Fibration db)
    primCompat proj conv = do
      result <- liftCompat $ project proj a1
      return $ first (\x -> if x then Just Map.empty else Nothing) $ conv result
    checkVisits :: CompatM db m (PossibleBindings db, Fibration db)
                -> CompatM db m (PossibleBindings db, Fibration db)
    checkVisits ans = if tpat `Set.member` visits
                        then return (Just Map.empty, Unexpanded)
                        else ans

liftCompat :: ProjM db m a -> CompatM db m a
liftCompat = id
