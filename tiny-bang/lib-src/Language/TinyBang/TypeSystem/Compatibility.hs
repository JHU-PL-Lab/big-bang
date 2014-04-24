{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, ViewPatterns, ConstraintKinds, TemplateHaskell #-}

{-|
  This module implements a computable function used for the compatibility
  relation in TinyBang typechecking.
-}
module Language.TinyBang.TypeSystem.Compatibility
( compatibility
, CompatibilityResult(..)
, CompatibilityCallContext(..)
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.Monad.Trans.NonDet
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger
import Utils.Monad.Writer

$(loggingFunctions)

data CompatibilityResult db =
  CompatibilityResult
    { compatibilitySlice :: Type db
    , compatibilityBindings :: Maybe db
    }

type CompatibilityConstraints db = (ConstraintDatabase db, Display db)

-- |Describes the context in which compatibility is invoked.
data CompatibilityCallContext db
  = CompatibilityCallContext
      { cxtCallScapeTypeOrVar :: TypeOrVar db
      , cxtCallArgTypeOrVar :: TypeOrVar db
      , cxtCallSiteVar :: TVar
      }
  deriving (Eq, Ord, Show)

{-|
  Determines if an argument is compatible with a pattern given the provided
  argument and pattern constraints. This function non-deterministically
  calculates all compatibility results which would make the compatibility
  relation hold under the current constraint set.  A compatibility result is a
  pair between a deep type (representing the union elimination of the original
  argument) and @Maybe@ a constraint database; this latter is @Nothing@ for
  incompatibility and @Just@ the set of type bindings for full compatibility.
  Partial compatibility yields no result (since partial compatibility never
  triggers any further behavior in the type system).
  
  The type of the provided argument is a @TypeOrVar@ potentially representing a
  deep type.  This allows the results of previous compatibility checks to be
  provided as arguments to compatibility, improving the efficiency of the
  implementation of matching by allowing it to skip branches which have already
  been captured by higher priority scapes.
  
  This function also accepts a 'CompatibilityCallContext' for history recording
  purposes.
-}
compatibility :: (CompatibilityConstraints db)
              => CompatibilityCallContext db
              -> TypeOrVar db
              -> db
              -> TVar
              -> db
              -> [CompatibilityResult db]
compatibility ccc argTypeOrVarVar argDb patVar patDb =
  runCompatibilityM ccc argDb patDb $ captureBindings argTypeOrVarVar patVar

newtype CompatibilityM db a
  = CompatibilityM
      { unCompatibilityM ::
          (WriterT CompatibilityAccumulation
            (NonDetT
              (Reader (CompatibilityContext db))))
          a
      }
  deriving ( Monad, Functor, Applicative
           , MonadReader (CompatibilityContext db)
           , MonadWriter CompatibilityAccumulation
           , MonadNonDet
           )

data CompatibilityContext db =
  CompatibilityContext
    { argdb :: db
    , patdb :: db
    , callContext :: CompatibilityCallContext db
    }

runCompatibilityM :: CompatibilityCallContext db
                  -> db
                  -> db
                  -> CompatibilityM db a
                  -> [a]
runCompatibilityM ccc db db' x =
  runReader (runNonDetT (fst <$> runWriterT (unCompatibilityM x)))
    (CompatibilityContext db db' ccc)

newtype CompatibilityAccumulation =
  CompatibilityAccumulation
    { openBindings :: Set TVar
    }
  deriving (Monoid)

{-|
  Captures the open bindings to the result type.  This is used to allow a sort
  of lazy binding of empty onion patterns; see the documentatin of
  'internalCompatibility' for more information.
-}
captureBindings :: forall db. (CompatibilityConstraints db)
                => TypeOrVar db
                -> TVar
                -> CompatibilityM db (CompatibilityResult db)
captureBindings tov0 a0' = do
  (result,acc) <- capture $ internalCompatibility tov0 a0'
  let CompatibilityResult
        { compatibilitySlice = t
        , compatibilityBindings = f } = result
  ccc <- callContext <$> ask
  let h = CompatibilityWiring
            (cxtCallScapeTypeOrVar ccc)
            (cxtCallArgTypeOrVar ccc)
            (cxtCallSiteVar ccc)
  let newBindings = map ((.: h) . (t <:)) $ Set.toList $ openBindings acc
  let f' = CDb.union (CDb.fromList newBindings) <$> f
  return CompatibilityResult
            { compatibilitySlice = t
            , compatibilityBindings = f' }

{-|
  Defines an internal compatibility function.  This routine addresses a number
  of complexities that arise when constructing a computable function from the
  compatibility relation specified in the theory.
  
  First, this implementation admits deep types in lieu of slices.  Using slices
  directly would introduce a problem in typechecking: equivalence between slices
  would need to be recognized in order to ensure decidability.  Because types
  may be deep, the argument at each step of compatibility may be a type *or* a
  variable.  This inner routine is flexible enough to address that situation.
  
  Second, slices in the theory have already been decided by the time
  compatibility starts, so the theory can immediately decide the appropriate
  bindings for an empty onion pattern.  Here, the slice is being deteremined as
  compatibility proceeds.  In order to bind correctly, empty onion patterns do
  not decide bindings immediately; instead, they return the variables that they
  would bind.  These variables are then bound at the top of onions by the
  @captureBindings@ function.  Since a pattern does not know when it is the top
  of an onion, it is the responsibility of the container to do this binding;
  for instance, a label always contains the top of an onion, so the label
  pattern will invoke @captureBindings@.
-}
internalCompatibility :: (CompatibilityConstraints db)
                      => TypeOrVar db
                      -> TVar
                      -> CompatibilityM db (CompatibilityResult db)
internalCompatibility tov0 a0' =
  bracketLogM _debugI
    (display $ text "Checking type compatibility of" <+>
                  makeDoc tov0 </> text "with pattern" <+> makeDoc a0')
    (\result ->
      let CompatibilityResult
            { compatibilitySlice = sliceType
            , compatibilityBindings = mcs
            } = result in 
      display $
        text "Type compatibility of" <+>
          makeDoc tov0 </> text "with pattern" <+> makeDoc a0' </>
          text "at slice" <+> makeDoc sliceType </>
          case mcs of
            Just cs -> text "gave binding constraints" <+> makeDoc cs
            Nothing -> text "was unsuccessful") $
    do
      -- Get a concrete lower bound for the pattern.  We do this first because
      -- conjunction patterns are always split before the arguments.
      db' <- patdb <$> ask
      {- NOTE: Here, we are assuming that variables in patterns have exactly one
         lower bound.  This would produce incorrect behavior if unions in
         patterns are used for e.g. disjunction.  This would be corrected by
         dropping the call to "choose" and addressing the set of lower bounds
         appropriately. -}
      t0' <- choose $ query db' $ QueryLowerBoundingTypesOfTVar a0'
      -- We now defer to another function to handle compatibility once the
      -- pattern type has been chosen.  (This allows recursion to enter at that
      -- point in other functions.)
      internalCompatibilityFixedPatternType tov0 a0' t0'

internalCompatibilityFixedPatternType
    :: forall db. (CompatibilityConstraints db)
    => TypeOrVar db
    -> TVar
    -> Type db
    -> CompatibilityM db (CompatibilityResult db)
internalCompatibilityFixedPatternType tov0 a0' t0' =
  bracketLogM _debugI
    (display $ text "Checking type compatibility of" <+> makeDoc tov0 </>
               text "with pattern" <+> makeDoc t0')
    (\r -> display $
        text "Type compatibility of" <+>
          makeDoc tov0 </> text "with pattern" <+> makeDoc t0' </>
          text "at slice" <+> makeDoc (compatibilitySlice r) </>
          case compatibilityBindings r of
            Just cs -> text "gave binding constraints" <+> makeDoc cs
            Nothing -> text "was unsuccessful")
    $
    -- We proceed based on the type of pattern.
    case t0' of
      TOnion tov1' tov2' -> do
        -- A conjunction pattern.  We must match both sides.
        CompatibilityResult
          { compatibilitySlice = t1
          , compatibilityBindings = f1
          } <- internalCompatibility tov0 (insistVar tov1')
        CompatibilityResult
          { compatibilitySlice = t2
          , compatibilityBindings = f2
          } <- internalCompatibility (mktov t1) (insistVar tov2')
        return CompatibilityResult
          { compatibilitySlice = t2
          , compatibilityBindings = CDb.union <$> f1 <*> f2
          }
      _ -> do
        -- It's not a conjunction pattern, so we can select a concrete lower bound
        -- now.
        t0 <- choose =<< queryLowerBoundsOfTypeOrVar <$> (argdb <$> ask) <*>
                return tov0
        let failure = return CompatibilityResult
                                { compatibilitySlice = t0
                                , compatibilityBindings = Nothing
                                }
        case (t0, t0') of
          (_, TOnion _ _) ->
            error "TOnion pattern should have been captured in previous case!"
          (_, TEmptyOnion) -> do
            tell CompatibilityAccumulation { openBindings = Set.singleton a0' }
            return CompatibilityResult
                      { compatibilitySlice = t0
                      , compatibilityBindings = Just CDb.empty
                      }
          (TEmptyOnion, _) ->
            failure
          (TPrimitive p, TPrimitive p') | p == p' ->
            return CompatibilityResult
                      { compatibilitySlice = t0
                      , compatibilityBindings = Just CDb.empty
                      }
          (TPrimitive _, _) ->
            failure
          (TLabel n tov1, TLabel n' tov1') | n == n' ->
            mapSlice (TLabel n . mktov) <$>
              captureBindings tov1 (insistVar tov1')
          (TLabel _ _, _) ->
            failure
          (TRef a1, TRef a1') -> do
            -- TODO: update this part if/when the model of state changes
            --   Currently, this code is just discarding the slice of the type
            --   under the cell.
            mdb <- compatibilityBindings <$> captureBindings (mktov a1) a1'
            return CompatibilityResult
                      { compatibilitySlice = TRef a1
                      , compatibilityBindings = mdb
                      }
          (TRef _, _) ->
            failure
          (TOnion tov1 tov2, _) -> do
            r <- internalCompatibilityFixedPatternType tov1 a0' t0'
            if isJust $ compatibilityBindings r
              then mapSlice (flip TOnion tov2 . mktov) <$> return r
              else mapSlice (TOnion tov1 . mktov) <$>
                      internalCompatibilityFixedPatternType tov2 a0' t0'
          (TScape{}, _) ->
            -- At the moment, the only thing which can match a scape is the empty
            -- onion pattern.
            failure
  where
    -- TODO: deprecate this function by using static typing to ensure that
    --       patterns never contain deep types.
    insistVar :: (Display db) => TypeOrVar db -> TVar
    insistVar (unTypeOrVar -> Right a) = a
    insistVar x = error $ "insistVar failure: " ++ display x
    mapSlice :: (Type db -> Type db)
             -> CompatibilityResult db
             -> CompatibilityResult db
    mapSlice f r = r { compatibilitySlice = f (compatibilitySlice r) }
