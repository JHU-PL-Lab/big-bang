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
import Control.Monad.State
import Data.List.Split
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree

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
    , compatibilityProof :: Proof db
    }

-- TODO: update this data structure from the formal thesis presentation?
data ProofRule db
  = CPPatternMatchFailure (Type db) (Type db)
    -- ^Represents an immediate pattern match failure.
  | CPPatternMatchSuccess (Type db) (Type db)
    -- ^Represents an immediate pattern match success.
  | CPLabel (Type db) (Type db) (Proof db)
    -- ^Represents a local label success.  The nested proof shows compatibility
    --  (or incompatibility) of the label contents, but this step asserts that
    --  the labels themselves match.
  | CPRef (Type db) (Type db) (Proof db)
    -- ^Represents a local ref success (as per label success).
  | CPConjunction (TypeOrVar db) (Type db) (Proof db) (Proof db)
    -- ^Represents a conjunction pattern proof.  The two subproofs show each
    --  side of the conjunction.
  | CPOnionLeft (Type db) (Type db) (Proof db)
    -- ^Represents an onion proof in which the left side matches.
  | CPOnionRight (Type db) (Type db) (Proof db) (Proof db)
    -- ^Represents an onion proof in which the left side does not match and so
    --  the proof result relies upon the right side.

data Proof db
  = CPStep
      { cpStepAnswer :: Bool
      , _cpStepRule :: ProofRule db
      }
    -- ^ Indicates a proof step.  The boolean indicates whether this subtree
    --   is a proof of success or a proof of failure.

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
compatibility :: forall db. (CompatibilityConstraints db)
              => CompatibilityCallContext db
              -> TypeOrVar db
              -> db
              -> TVar
              -> db
              -> [CompatibilityResult db]
compatibility ccc argTypeOrVar argDb patVar patDb =
  let rs = runCompatibilityM ccc argDb patDb $
              captureBindings argTypeOrVar patVar in
  showProofsAndReturn rs
  where
    showProofsAndReturn :: [CompatibilityResult db] -> [CompatibilityResult db]
    showProofsAndReturn rs =
      _debugI
        (display $ text "Compatibility check:" </>
                      indent 2 (align $
                        text "Argument:" <+> makeDoc argTypeOrVar </>
                        text "Pattern:" <+> makeDoc patVar <+>
                          text "\\" <+> makeDoc patDb </>
                        text "Proofs:" </>
                          indent 2 (align $ vcat $
                            map (makeDocFrom2DString . drawTree .
                                    fmap display . treeFromProof .
                                    compatibilityProof) rs)))
        rs
    makeDocFrom2DString :: String -> Doc
    makeDocFrom2DString s = align $ vcat $ map text $ splitOn "\n" s
    treeFromProof :: Proof db -> Tree Doc
    treeFromProof (CPStep success rule) =
      let t = treeFromProofRule rule in
      let tag = if success then "(compat)" else "(incompat)" in
      t { rootLabel = text tag <+> rootLabel t }
    treeFromProofRule :: ProofRule db -> Tree Doc
    treeFromProofRule rule = case rule of
      CPPatternMatchFailure t0 t0' ->
        Node {
          rootLabel = stdtag "Immediate failure:" t0 t0',
          subForest = []
          }
      CPPatternMatchSuccess t0 t0' ->
        Node {
          rootLabel = stdtag "Immediate success:" t0 t0',
          subForest = []
          }
      CPLabel t0 t0' p ->
        Node {
          rootLabel = stdtag "Label success:" t0 t0',
          subForest = [treeFromProof p]
          }
      CPRef t0 t0' p ->
        Node {
          rootLabel = stdtag "Ref success:" t0 t0',
          subForest = [treeFromProof p]
          }
      CPConjunction t0 t0' p1 p2 ->
        Node {
          rootLabel = stdtag "Conjunctive proof:" t0 t0',
          subForest = map treeFromProof [p1,p2]
          }
      CPOnionLeft t0 t0' p ->
        Node {
          rootLabel = stdtag "Onion left proof:" t0 t0',
          subForest = [treeFromProof p]
          }
      CPOnionRight t0 t0' p1 p2 ->
        Node {
          rootLabel = stdtag "Onion right proof:" t0 t0',
          subForest = map treeFromProof [p1,p2]
          }
      where
        stdtag :: (Display a, Display b) => String -> a -> b -> Doc
        stdtag pfx t0 t0' =
          text pfx <+> makeDoc t0 <+> text "with" <+> makeDoc t0'

newtype CompatibilityM db a
  = CompatibilityM
      { unCompatibilityM ::
          (StateT (CompatibilityState db)
            (WriterT CompatibilityAccumulation
              (NonDetT
                (Reader (CompatibilityContext db)))))
          a
      }
  deriving ( Monad, Functor, Applicative
           , MonadReader (CompatibilityContext db)
           , MonadWriter CompatibilityAccumulation
           , MonadState (CompatibilityState db)
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
  let initialState =
        CompatibilityState { encounteredLowerBounds = Set.empty } in
  let initialContext =
        CompatibilityContext db db' ccc in
  runReader
    (runNonDetT
      (fst <$> runWriterT
        (evalStateT (unCompatibilityM x)
          initialState)))
    initialContext

newtype CompatibilityAccumulation =
  CompatibilityAccumulation
    { openBindings :: Set TVar
    }
  deriving (Monoid)

newtype CompatibilityState db =
  CompatibilityState
    { encounteredLowerBounds :: Set (Type db)
    }

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
  return result { compatibilityBindings = f' }

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
          , compatibilityProof = p1
          } <- internalCompatibility tov0 (insistVar tov1')
        CompatibilityResult
          { compatibilitySlice = t2
          , compatibilityBindings = f2
          , compatibilityProof = p2
          } <- internalCompatibility (mktov t1) (insistVar tov2')
        return CompatibilityResult
          { compatibilitySlice = t2
          , compatibilityBindings = CDb.union <$> f1 <*> f2
          , compatibilityProof =
              CPStep (cpStepAnswer p1 && cpStepAnswer p2) $
                CPConjunction tov0 t0' p1 p2
          }
      _ -> do
        -- It's not a conjunction pattern, so we can select a concrete lower bound
        -- now.
        t0 <- choose =<< queryLowerBoundsOfTypeOrVar <$> (argdb <$> ask) <*>
                return tov0
        let failure = return CompatibilityResult
                                { compatibilitySlice = t0
                                , compatibilityBindings = Nothing
                                , compatibilityProof =
                                    CPStep False $ CPPatternMatchFailure t0 t0'
                                }
        case (t0, t0') of
          (_, TOnion _ _) ->
            error "TOnion pattern should have been captured in previous case!"
          (_, TEmptyOnion) -> do
            tell CompatibilityAccumulation { openBindings = Set.singleton a0' }
            return CompatibilityResult
                      { compatibilitySlice = t0
                      , compatibilityBindings = Just CDb.empty
                      , compatibilityProof =
                          CPStep True $ CPPatternMatchSuccess t0 t0'
                      }
          (TEmptyOnion, _) ->
            failure
          (TPrimitive p, TPrimitive p') | p == p' ->
            return CompatibilityResult
                      { compatibilitySlice = t0
                      , compatibilityBindings = Just CDb.empty
                      , compatibilityProof =
                          CPStep True $ CPPatternMatchSuccess t0 t0'
                      }
          (TPrimitive _, _) ->
            failure
          (TLabel n tov1, TLabel n' tov1') | n == n' -> do
            r <- captureBindings tov1 (insistVar tov1')
            return r { compatibilitySlice =
                          TLabel n $ mktov $ compatibilitySlice r
                     , compatibilityProof =
                          CPStep (isJust $ compatibilityBindings r) $
                            CPLabel t0 t0' $ compatibilityProof r
                     }
          (TLabel _ _, _) ->
            failure
          (TRef a1, TRef a1') -> do
            -- TODO: update this part if/when the model of state changes
            --   Currently, this code is just discarding the slice of the type
            --   under the cell.
            r <- captureBindings (mktov a1) a1'
            let mdb = compatibilityBindings r 
            return CompatibilityResult
                      { compatibilitySlice = TRef a1
                      , compatibilityBindings = mdb
                      , compatibilityProof =
                          CPStep (isJust mdb) $ CPRef t0 t0' $
                            compatibilityProof r
                      }
          (TRef _, _) ->
            failure
          (TOnion tov1 tov2, _) -> do
            r <- internalCompatibilityFixedPatternType tov1 a0' t0'
            if isJust $ compatibilityBindings r
              then
                return CompatibilityResult
                       { compatibilitySlice =
                            flip TOnion tov2 $ mktov $ compatibilitySlice r
                        , compatibilityBindings = compatibilityBindings r
                       , compatibilityProof =
                            CPStep (cpStepAnswer $ compatibilityProof r) $
                              CPOnionLeft t0 t0' $ compatibilityProof r
                       }
              else do
                r' <- internalCompatibilityFixedPatternType tov2 a0' t0'
                return CompatibilityResult
                          { compatibilitySlice =
                              TOnion (mktov $ compatibilitySlice r)
                                     (mktov $ compatibilitySlice r')
                          , compatibilityBindings = compatibilityBindings r'
                          , compatibilityProof =
                              CPStep (cpStepAnswer $ compatibilityProof r) $
                                CPOnionRight t0 t0' (compatibilityProof r)
                                                    (compatibilityProof r')
                          } 
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
