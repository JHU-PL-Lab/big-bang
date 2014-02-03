{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TupleSections, TemplateHaskell #-}

{-|
  This module defines a computable function which calculates the appropriate
  results for the matching relation.
-}
module Language.TinyBang.TypeSystem.Matching
( matches
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Compatibility
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.NonDet
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

type MatchResult db = (TypeOrVar db, Maybe (Type db, (TVar, db)))

{-|
  Defines the computable function which determines matching for the TinyBang
  type system implementation.  This function accepts a function type and an
  argument type (along with their associated constraints) and produces a list
  of matching results.
  
  A matching result is a tuple between an argument type or variable and @Maybe@
  a dispatch pair.  If the dispatch pair is @Nothing@, then the provided
  argument cannot be matched to any scape at this call site.  Otherwise, the
  dispatch pair is @Just@ the concrete scape type which matched the argument and
  a constrained type representing the bindings which should be used to dispatch
  on it.
  
  This function also accepts as its first argument a type variable representing
  the call site.  This is for accounting purposes; it affects the history of the
  generated constraints but nothing more.
-}
matches :: (ConstraintDatabase db, Display db)
        => TVar -> TVar -> TVar -> db -> [MatchResult db]
matches callSiteVar a0 a1 db =
  runMatchesM (matchesInternal (mktov a0) (mktov a1)) callSiteVar db

newtype MatchesM db a
  = MatchesM
      { unMatchesM :: (StateT (MatchesState db)
                        (NonDetT
                          (ReaderT (MatchesContext db)
                            (CReader db))))
                      a }
      deriving ( Monad, Functor, Applicative, MonadCReader db
               , MonadNonDet, MonadReader (MatchesContext db)
               , MonadState (MatchesState db))

data MatchesContext db =
  MatchesContext
    { matchesCallSiteVar :: TVar
    }
    
data MatchesState db =
  MatchesState
    { visitedScapes :: Set (Type db)
    }

runMatchesM :: (ConstraintDatabase db) => MatchesM db a -> TVar -> db -> [a]
runMatchesM x callSiteVar =
  let context = MatchesContext callSiteVar in
  let initState = MatchesState Set.empty in
  runCReader
    (runReaderT
      (runNonDetT
        (evalStateT
          (unMatchesM x)
          initState
        )
      )
      context
    )

-- |Performs the matching operation under a suitable monad.  The monad tracks
--  the in-context constraint set for the variables, the call site history info,
--  and the list of scapes already visited by this procedure.  It also handles
--  non-determinism.
matchesInternal :: forall db. (ConstraintDatabase db, Display db)
                => TypeOrVar db -> TypeOrVar db -> MatchesM db (MatchResult db)
matchesInternal tov0 tov1 =
  bracketLogM _debugI
    (display $ text "Checking matches for function" <+> makeDoc tov0 <+>
               text "and argument" <+> makeDoc tov1)
    (\(slc, mout) -> display $ text "Matching of function" <+> makeDoc tov0 <+>
                               text "and argument" <+> makeDoc tov1 <+>
      case mout of
        Nothing -> text "failed with slice" <+> makeDoc slc
        Just (tfn, _) -> text "succeeded with slice" <+> makeDoc slc <+>
                              text "at concrete function type" <+>
                              makeDoc tfn) $
  do
    db <- askDb
    t <- postLogM _debugI
          (\t -> display $ text "Checking matches for chosen function" <+>
                           makeDoc t <+> text "and argument" <+> makeDoc tov1) $
          choose $ queryLowerBoundsOfTypeOrVar db tov0
    alreadyVisited <- Set.member t <$> visitedScapes <$> get
    if alreadyVisited then return (tov1, Nothing) else
      case t of
        TScape a' patdb a bodydb -> do
          modify $ \s -> s { visitedScapes = Set.insert t $ visitedScapes s }
          ccc <- CompatibilityCallContext tov0 tov1 <$>
                    (matchesCallSiteVar <$> ask)
          (argt,mbinds) <- choose $ compatibility ccc tov1 db a' patdb
          return (mktov argt, (t,) <$> (a,) . CDb.union bodydb <$> mbinds) 
        TOnion a2 a3 -> do
          {- PERF: do something to prevent duplicate work
              e.g. let f and g be functions with pattern int and let fOrG be
                   their union; let h be a function with pattern char; then
                     (fOrG & h) 'z'
                   will slice the argument once for f and once for g,
                   propagating both back to h independently; this could be quite
                   expensive if the onion is quite deep (e.g. object with
                   dynamic mixin) and is definitely redundant if the slices are
                   the same
          -}
          r <- matchesInternal a2 tov1
          if isJust $ snd r
            then return r
            else matchesInternal a3 $ fst r
        _ ->
          return (tov1, Nothing)
