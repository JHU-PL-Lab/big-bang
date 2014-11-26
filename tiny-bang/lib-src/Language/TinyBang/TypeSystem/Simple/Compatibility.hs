{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, ViewPatterns, TemplateHaskell #-}
module Language.TinyBang.TypeSystem.Simple.Compatibility
( findCompatibilityCases
, sensible
) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.NonDet
import qualified Data.Foldable as Foldable
import Data.Monoid hiding ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.TypeSystem.Simple.Data
import Language.TinyBang.Utils.Display as D
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

-- TODO: freak out on non-contractive types

-- |A computable function modeling the behavior of compatibility in the type
--  system.
findCompatibilityCases :: TVar
                            -- ^The argument variable
                       -> ConstraintSet
                            -- ^The current constraint set in closure.
                       -> PatternType
                            -- ^The pattern which should be bound.
                       -> PatternTypeSet
                            -- ^The patterns which have been excluded and should
                            --  not match.
                       -> Set (Maybe ConstraintSet)
                            -- ^The resulting sets of constraints which
                            --  represent successful compatibility proofs given
                            --  the provided parameters.  A @Nothing@ indicates
                            --  a case in which the argument did not match the
                            --  provided binding pattern.
findCompatibilityCases a cs pB psN =
  let cpatB = CompatibilityPattern pB True Nothing in
  let cpatsN = Seq.fromList $ map (makeCompatibilityPattern False False) $
                  Set.toList $ unPatternTypeSet psN in
  let x = compatibilityTVar a (cpatB <| cpatsN) DoBind in
  Set.fromList $ map processResult $ runCompatM cs x
  where
    processResult (bindings,answers) =
      let (ans :< _) = viewl answers in
      if ans then Just bindings else Nothing

-- |A computable function to determine whether a given restricted type is
--  "sensible" according to the theory.
sensible :: FilteredType -> ConstraintSet -> Bool
sensible (FilteredType t patsP patsN) cs =
  let cpatsP = Seq.fromList $ map (makeCompatibilityPattern False True) $
                  Set.toList $ unPatternTypeSet patsP in
  let cpatsN = Seq.fromList $ map (makeCompatibilityPattern False False) $
                  Set.toList $ unPatternTypeSet patsN in
  let x = compatibilityType t (cpatsP >< cpatsN) DoBind in
  not $ Prelude.null $ runCompatM cs x

-- |A utility function shared by @findCompatibleBindings@ and @sensible@.
makeCompatibilityPattern :: Bool -> Bool -> PatternType
                         -> CompatibilityPattern
makeCompatibilityPattern binding expectMatch pattern =
  CompatibilityPattern pattern binding $ Just expectMatch

-- |The compatibility monad.  The reader provides a global constraint set.
--  Non-determinism is provided when lower bounds must be selected.
newtype CompatM a
  = CompatM { unCompatM :: ReaderT (Set CompatibilityOccurrence)
                              (NonDetT (Reader ConstraintSet)) a
            }
  deriving ( Functor, Applicative, Monad, MonadPlus
           , MonadReader (Set CompatibilityOccurrence)
           , MonadNonDet)

runCompatM :: ConstraintSet -> CompatM a -> [a]
runCompatM cs x =
  runReader (runNonDetT $ runReaderT (unCompatM x) Set.empty) cs

withOccurrence :: CompatibilityOccurrence -> CompatM a -> CompatM a
withOccurrence occ = local (Set.insert occ)

hasOccurrence :: CompatibilityOccurrence -> CompatM Bool
hasOccurrence occ = Set.member occ <$> ask

globalConstraints :: CompatM ConstraintSet
globalConstraints = CompatM $ lift $ lift ask

-- |A structure to describe the expectations of patterns as compatibility
--  proceeds.
data CompatibilityPattern
  = CompatibilityPattern
      { cpPat :: PatternType
          -- ^The pattern to check.
      , cpBinding :: Bool
          -- ^True if a match should generate bindings; False otherwise.
      , cpExpectMatch :: Maybe Bool
          -- ^Our requirement for matching.  Just True indicates that the
          --  pattern must match; Just False indicates that it must not.
          --  Nothing indicates no expectation.
      }
  deriving (Eq, Ord, Show)

data BindState
  = DontBind -- ^The "bind needed" symbol in the theory.  Indicates that the
             --  result of the proof may not have all of its bindings.  From
             --  the point of view of the computable function, this means that
             --  no binding needs to be done.
  | DoBind   -- ^The "bind complete" symbol in the theory.  Indicates that the
             --  result of the proof must have all bindings.  From the point of
             --  view of the computable function, this means that binding must
             --  be finished upon returning.
  deriving (Eq, Ord, Show)

-- |A data structure representing an occurrence check to eliminate cycles in
--  compatibility proofs.  Due to how the Type Selection rule introduces new
--  proof obligations (in the form of patterns), it is possible within a proof
--  tree to reach the same type selection as has already been reached.  Because
--  all rules are idempotent in their effect on the result, it is sound to
--  eliminate such cycles.  We capture the proof obligations at each type
--  selection within this data structure and use a set of them to determine
--  when a cycle has occurred.
data CompatibilityOccurrence
  = CompatibilityOccurrence
      { coTypeSelected :: FilteredType
      , coUpperBound :: TVar
      , coCpats :: Set CompatibilityPattern
      }
  deriving (Eq, Ord, Show)

-- |Computes a compatibility check with a given type variable.  This function
--  accepts a single collection of patterns, each tagged with information about
--  its binding and matching properties.  This collection is a list so that its
--  ordering can be used to control the ordering of the result.
compatibilityTVar :: TVar
                      -- ^The argument type variable.
                  -> Seq CompatibilityPattern
                      -- ^The patterns to check.
                  -> BindState
                      -- ^The current binding state.
                  -> CompatM ( ConstraintSet
                             , Seq Bool
                             )
                      -- ^The resulting set of bindings and a list of
                      --  boolean values indicating whether or not each
                      --  pattern matched.
compatibilityTVar a cpats bindState =
  bracketLogM _debugI 
    (display $ text "Starting compatibilityTVar with" <+> makeDoc a <+>
               text "and patterns:" </>
                indent 2 (
                  foldl (</>) D.empty $ map makeDoc $
                    Foldable.toList cpats
                )
    )
    (\(cs, answers) -> display $
      text "compatibilityTVar results with type" <+> makeDoc a <> char ':' </>
      indent 2
        (let f (cpat,ans) =
                text (if ans then "Success" else "Failure") <+>
                text "on" <+> makeDoc cpat
         in foldl (</>) D.empty $ map f $ Foldable.toList $
            Seq.zip cpats answers
        ) </>
      text "with bindings" </>
      indent 2 (makeDoc cs)
    )
  $
  if Seq.null cpats
    then
      -- ## Leaf rule
      return (ConstraintSet Set.empty, Seq.empty)
    else do
      -- ## Type Selection rule
      cs <- unConstraintSet <$> globalConstraints
      LowerBoundConstraint ft@(FilteredType t fpP fpN) a' <- choose cs
      guard $ a == a'
      let fpPcpats = Seq.fromList $ map (mkCpat $ Just True) $
                        Set.toList $ unPatternTypeSet fpP
      let fpNcpats = Seq.fromList $ map (mkCpat $ Just False) $
                        Set.toList $ unPatternTypeSet fpN
      -- This is the bit where we do our occurrence check.
      let occurrence = CompatibilityOccurrence
                          { coTypeSelected = ft
                          , coUpperBound = a
                          , coCpats = Set.unions $
                              map (Set.fromList . Foldable.toList)
                              [ cpats
                              , fpPcpats
                              , fpNcpats
                              ]
                          }
      -- If the occurrence has already appeared in a previous recursion, then
      -- this state represents a cycle and can be destroyed.
      join $ guard . not <$> hasOccurrence occurrence
      -- Otherwise, proceed using this occurrence so we can recognize it if it
      -- appears in this (mutually) recursive call. 
      (bindings, answers) <- withOccurrence occurrence $
          compatibilityType t (cpats >< fpPcpats >< fpNcpats) bindState
      -- Now provide the results (eliminating the answers for the patterns which
      -- were involved just because of the type we selected).
      return (bindings, Seq.take (Seq.length cpats) answers)
  where
    mkCpat :: Maybe Bool -> PatternType -> CompatibilityPattern
    mkCpat mexpect pat =
      CompatibilityPattern { cpPat = pat
                           , cpBinding = False
                           , cpExpectMatch = mexpect
                           }

-- |Computes a compatibility check with a given concrete type.
compatibilityType :: Type
                  -> Seq CompatibilityPattern
                  -> BindState
                  -> CompatM ( ConstraintSet
                             , Seq Bool
                             )
compatibilityType tIn cpatsIn bindState =
  bracketLogM _debugI
    ( display $ text "Starting compatibilityType with type" <+> makeDoc tIn <+>
      text "and patterns:" </>
      indent 2 (foldl (</>) D.empty $ map makeDoc $ Foldable.toList cpatsIn)
    )
    (\(cs, answers) -> display $
      text "compatibilityType results with type" <+> makeDoc tIn <> char ':' </>
      indent 2
        (let f (cpat,ans) =
                text (if ans then "Success" else "Failure") <+>
                text "on" <+> makeDoc cpat
         in foldl (</>) D.empty $ map f $ Foldable.toList $
            Seq.zip cpatsIn answers
        ) </>
      text "with bindings" </>
      indent 2 (makeDoc cs)
    )
  $
  do
    -- We proceed first by operating on the non-constructor patterns (empty
    -- and conjunction) and then by working on constructor patterns in tandem.
    (bindings, answers, answersEmpty) <- compatibilityTypeMaybeWithNonConstrPats
                                            tIn cpatsIn Seq.empty
    unless (Seq.null answersEmpty) $
      error "answersEmpty not empty in type compatibility!"
    let newBindings = ConstraintSet $
          case bindState of
            DontBind -> Set.empty
            DoBind ->
              -- Since we need to do binding here, we create the appropriate lower
              -- bounds for each binding pattern variable which *matched*.  If the
              -- binding pattern did not match, no bindings are generated.
              let pairs = Seq.zip cpatsIn answers in
              let seqToSet = PatternTypeSet . Set.fromList .
                                map (cpPat . fst) . Foldable.toList in
              let (posPats,negPats) = seqToSet *** seqToSet $
                                          Seq.partition snd pairs in
              Set.unions $ Foldable.toList $ fmap
                (\(cpat,ans) ->
                  if cpBinding cpat && ans
                    then
                      let ft = FilteredType tIn posPats negPats in
                      let (PatternType a _) = cpPat cpat in
                      Set.singleton $ ft <: a
                    else
                      Set.empty
                ) $ Seq.zip cpatsIn answers
    return (bindings `mappend` newBindings, answers)
  where
    -- |Breaks down each pattern until all of them are in constructor pattern
    --  form.
    compatibilityTypeMaybeWithNonConstrPats :: Type
                                            -> Seq CompatibilityPattern
                                            -> Seq CompatibilityPattern
                                            -> CompatM ( ConstraintSet
                                                       , Seq Bool
                                                       , Seq Bool
                                                       )
    compatibilityTypeMaybeWithNonConstrPats t cpats cpatsCtrOnly =
      bracketLogM _debugI
        (display $
          text "Starting compatibilityTypeMaybeWithNonConstrPats with type" <+>
          makeDoc t <+> text "and" <+>
          indent 2 (
            text "constructor-only patterns:" <+>
              makeDoc (Foldable.toList cpatsCtrOnly) </>
            text "general patterns:" <+>
              makeDoc (Foldable.toList cpats)
          )
        )
        (\(cs, answers, answersCtrOnly) -> display $
          text "compatibilityTypeMaybeWithNonConstrPats" </>
          indent 2 (
            text "takes inputs:" </>
            indent 2 (
              text "type:" <+> makeDoc t </>
              text "constructor-only patterns:" <+>
                makeDoc (Foldable.toList cpatsCtrOnly) </>
              text "general patterns:" <+>
                makeDoc (Foldable.toList cpats)
            ) </>
            text "and yields:" </>
            indent 2 (
              text "constructor-only answers:" <+>
                makeDoc (Foldable.toList answersCtrOnly) </>
              text "general answers:" <+> makeDoc (Foldable.toList answers) </>
              text "bindings:" <+> makeDoc cs
            )
          )
        ) $
      case Seq.viewl cpats of
        EmptyL -> do
          (bindings, answers) <- compatibilityTypeOnlyConstrPats t cpatsCtrOnly
          return (bindings, Seq.empty, answers)
        cpat :< cpats' ->
          let PatternType a pftm = cpPat cpat in
          let filt = pfLookup a pftm in
          _debugI (display $ text "compatibilityTypeMaybeWithNonConstrPats:" <+>
                    text "Proceeding with" <+> makeDoc filt) $
          case filt of
            TFEmpty -> do
              -- ## Empty Pattern rule: always matches
              (bindings, answers, answersCtrOnly) <-
                  compatibilityTypeMaybeWithNonConstrPats t cpats' cpatsCtrOnly
              guardMatch True cpat
              return (bindings, True <| answers, answersCtrOnly)
            TFConjunction a1 a2 -> do
              -- ## Conjunction Pattern rule
              let (leftExpect, rightExpect) =
                    case cpExpectMatch cpat of
                      Nothing -> (Nothing, Nothing)
                      Just True -> (Just True, Just True)
                      Just False -> (Nothing, Nothing)
              let lCpat = CompatibilityPattern { cpPat = PatternType a1 pftm
                                               , cpBinding = cpBinding cpat
                                               , cpExpectMatch = leftExpect
                                               }
              let rCpat = CompatibilityPattern { cpPat = PatternType a2 pftm
                                               , cpBinding = cpBinding cpat
                                               , cpExpectMatch = rightExpect
                                               }              
              (bindings, answers, answersCtrOnly) <-
                  compatibilityTypeMaybeWithNonConstrPats t
                    (lCpat <| rCpat <|cpats') cpatsCtrOnly
              -- Because we put two new patterns in, we should get two new
              -- patterns out.
              let (Seq.viewl -> lans :< (Seq.viewl -> rans :< ans')) = answers
              let successful = lans && rans
              guardMatch successful cpat
              return (bindings, successful <| ans', answersCtrOnly)
            _ -> do
              -- Pattern is a data constructor pattern.  Pass it along.
              (bindings, answers, answersCtrOnly) <-
                compatibilityTypeMaybeWithNonConstrPats t
                  cpats' (cpatsCtrOnly |> cpat)
              let (answersCtrOnly' :> ans) = Seq.viewr answersCtrOnly
              return (bindings, ans <| answers, answersCtrOnly')
          
    -- |Performs pattern matching on a concrete type with the invariant that the
    --  patterns are all type constructor patterns.
    compatibilityTypeOnlyConstrPats :: Type
                                    -> Seq CompatibilityPattern
                                    -> CompatM ( ConstraintSet
                                               , Seq Bool
                                               )
    compatibilityTypeOnlyConstrPats t cpats =
      bracketLogM _debugI
        (display $
          text "Starting compatibilityTypeOnlyConstrPats with type" <+>
          makeDoc t <+> text "and" <+>
          indent 2 (
            text "constructor-only patterns:" <+>
              makeDoc (Foldable.toList cpats)
          )
        )
        (\(cs, answers) -> display $
          text "compatibilityTypeOnlyConstrPats" </>
          indent 2 (
            text "takes inputs:" </>
            indent 2 (
              text "type:" <+> makeDoc t </>
              text "constructor-only patterns:" <+>
                makeDoc (Foldable.toList cpats)
            ) </>
            text "and yields:" </>
            indent 2 (
              text "answers:" <+> makeDoc (Foldable.toList answers) </>
              text "bindings:" <+> makeDoc cs
            )
          )
        ) $
      case t of
        TPrimitive tprim ->
          let answerPrimPat (PatternType a pftm) =
                let filt = pfLookup a pftm in
                case filt of
                  TFPrim tprim' -> tprim == tprim'
                  _ -> False
          in
          immediateSolve answerPrimPat cpats
        TEmptyOnion ->
          immediateSolve (const False) cpats
        TLabel n a ->
          recursiveSingleConstructorSolve a
            (\filt -> return $ case filt of
                        TFLabel n' a' | n == n' -> Just a'
                        _ -> Nothing)
            cpats
            DoBind
        TRef a ->
          recursiveSingleConstructorSolve a
            (\filt -> return $ case filt of
                        TFRef a' -> Just a'
                        _ -> Nothing)
            cpats
            DoBind
        TOnion a1 a2 -> do
          -- The strategy here is to process the patterns as follows: every
          -- expected failure is sent as an expected failure and every other
          -- pattern (expected success or no expectation) is sent with no
          -- expectation to a recursive query on the left onion variable.  Then,
          -- a second call is made with each expected negative as a negative and
          -- each other *failed* pattern with its original expectation.  The
          -- results are then aggregated.
          (bindingsLeft, rightSideTasks) <- recursiveSolve a1
            (\cpat ->
                let leftSidePattern =
                      CompatibilityPattern
                        { cpPat = cpPat cpat
                        , cpBinding = cpBinding cpat
                        , cpExpectMatch =
                            if cpExpectMatch cpat == Just False
                              then Just False
                              else Nothing
                        } in 
                return $ RecursiveCompatQuestion
                  -- This is the question for the left side
                  leftSidePattern $
                  -- Given the left side questions, here are the tasks for the
                  -- right side.
                  \ans ->
                    if cpExpectMatch cpat == Just False
                      then do
                        -- If we expected a match failure and got a success,
                        -- we can't build this proof; we're done.
                        when ans mzero
                        -- Otherwise, we need to produce a task which will
                        -- confirm that this pattern also does not match the
                        -- right side.  It turns out that the left side pattern
                        -- suffices here.
                        return $ RecursiveCompatQuestion leftSidePattern $
                                    guardMatchReturn cpat
                      else
                        -- Here, we didn't actively expect anything.  But if
                        -- we got a success, this pattern's done and we can
                        -- move on.  Otherwise, we have to try to match in the
                        -- right side now.
                        if ans
                          then
                            -- We were successful; this pattern matches.
                            return $ ImmediateAnswer True
                          else
                            -- The pattern didn't match the left side.  We
                            -- now wish to show it is satisfied by the right
                            -- side.  It turns out that using the original
                            -- pattern on the right side (with its same
                            -- binding and expectation properties) is sufficient
                            -- to get the behavior we want.
                            return $ RecursiveCompatQuestion cpat $
                                        guardMatchReturn cpat
            )
            cpats
            DontBind
          (bindingsRight, answers) <- recursiveSolve a2 return rightSideTasks
                                        DontBind
          return (bindingsLeft `mappend` bindingsRight, answers)
        TScape _ _ _ ->
          immediateSolve (const False) cpats
      where
        -- |Solves each compatibility pattern immediately.  Given the sequence
        --  of inputs and a handler for a single pattern type, this function
        --  will produce a sequence of answers.
        immediateSolve :: (PatternType -> Bool)
                       -> Seq CompatibilityPattern
                       -> CompatM (ConstraintSet, Seq Bool)
        immediateSolve f cpats' =
          let g cpat = guardMatchReturn cpat $ f $ cpPat cpat
          in
          (ConstraintSet Set.empty,) <$> Seq.fromList <$>
            mapM g (Foldable.toList cpats')
        -- |Solves each compatibility pattern for a single-argument type
        --  constructor.  This function takes a filter type handler which yields
        --  an inner variable on which to recurse.  It performs this recursion
        --  where appropriate.  When the recursive match is successful, the
        --  larger match is successful; if it is not or if the handler did not
        --  yield an inner variable, the larger match is assumed to fail.
        recursiveSingleConstructorSolve :: TVar
                                        -> (FilterType -> CompatM (Maybe TVar))
                                        -> Seq CompatibilityPattern
                                        -> BindState
                                        -> CompatM (ConstraintSet, Seq Bool)
        recursiveSingleConstructorSolve a unwrap cpats' bindState' =
          let f cpat = do
                let (PatternType a'' pftm) = cpPat cpat
                let filt = pfLookup a'' pftm
                ma <- unwrap filt
                case ma of
                  Just a' -> 
                    let cpat' = CompatibilityPattern
                                  { cpPat = PatternType a' pftm
                                  , cpBinding = cpBinding cpat
                                  , cpExpectMatch = cpExpectMatch cpat
                                  } in
                    return $ RecursiveCompatQuestion cpat' $
                              guardMatchReturn cpat'
                  Nothing ->
                    ImmediateAnswer <$> guardMatchReturn cpat False
          in
          recursiveSolve a f cpats' bindState'
        -- |Solves a recursive compatibility question.  Given a sequence of
        --  information (e.g. patterns) and a task generation function which
        --  yields recursive compatibility questions, this function executes a
        --  single recursive compatibility check and then aligns each recursive
        --  answer with the position of the input which generated the question.
        recursiveSolve :: TVar
                       -> (a -> CompatM (CompatTask b))
                       -> Seq a
                       -> BindState
                       -> CompatM (ConstraintSet, Seq b)
        recursiveSolve a taskFn inputs bindState' = do
          tasks <- mapM taskFn $ Foldable.toList inputs
          let getQuestion task = case task of
                                    RecursiveCompatQuestion q _ -> Just q
                                    _ -> Nothing
          let recQs = Seq.fromList $ mapMaybe getQuestion tasks
          (bindings, subAnswers) <- compatibilityTVar a recQs bindState'
          let deriveAnswer :: CompatTask a -> State (Seq Bool) (CompatM a)
              deriveAnswer task =
                case task of
                  RecursiveCompatQuestion _ f -> do
                    (ans :< subAnswersLeft) <- Seq.viewl <$> get
                    put subAnswersLeft
                    return $ f ans
                  ImmediateAnswer ans ->
                    return $ return ans
          answers <- sequence $ evalState (mapM deriveAnswer tasks) subAnswers
          return (bindings, Seq.fromList answers)
        -- |A simple utility function to check a compatibility answer against
        --  an expectation and then return it.
        guardMatchReturn :: CompatibilityPattern -> Bool -> CompatM Bool
        guardMatchReturn cpat ans = guardMatch ans cpat >> return ans

data CompatTask a
  = RecursiveCompatQuestion CompatibilityPattern (Bool -> CompatM a)
  | ImmediateAnswer a

guardMatch :: (Monad m, MonadPlus m) => Bool -> CompatibilityPattern -> m ()
guardMatch expected pat =
  case cpExpectMatch pat of
    Nothing -> return ()
    Just expected' -> guard $ expected == expected'

pfLookup :: TVar -> Map TVar FilterType -> FilterType
pfLookup a pftm =
  fromMaybe
    (error $ "Pattern variable with no bound: " ++ display (PatternType a pftm))
    (Map.lookup a pftm)

instance Display CompatibilityPattern where
  makeDoc cpat =
    text (if cpBinding cpat then "binding" else "non-binding") <+>
    text "pattern" <+>
    (case cpExpectMatch cpat of
      Nothing -> D.empty
      Just True -> text "expecting match"
      Just False -> text "expecting failure"
    ) <> char ':' <+> makeDoc (cpPat cpat)
