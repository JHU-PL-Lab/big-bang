{-# LANGUAGE TupleSections, TypeFamilies, FlexibleInstances, FlexibleContexts, ImplicitParams, TypeSynonymInstances #-}
module Language.TinyBang.Types.Closure
( calculateClosure
) where

import Language.TinyBang.Types.Types ( (<:)
                                     , (.:)
                                     , Constraints
                                     , Constraint(..)
                                     , TauDown(..)
                                     , TauUp(..)
                                     , TauChi(..)
                                     , ConstraintHistory(..)
                                     , PolyFuncData(..)
                                     , Guard(..)
                                     , PrimitiveType(..)
                                     , SubTerm(..)
                                     , ForallVars
                                     , Cell(..)
                                     , CellGet(..)
                                     , CellSet(..)
                                     , InterAlpha
                                     , InterAlphaChain (..)
                                     , CellAlpha
                                     , CellAlphaChain (..)
                                     , SomeAlpha
                                     , AlphaType
                                     , AnyAlpha
                                     , AlphaSubstitutionEnv
                                     , substituteAlphaHelper
                                     )

import Data.Function.Utils (leastFixedPoint)
import Data.Set.Utils (singIf)

import Debug.Trace (trace)
import Utils.Render.Display (display)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, catMaybes)
import Control.Monad.Reader (runReader, ask, local, Reader, MonadReader)
import Control.Monad (guard, join, mzero)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (first, second, (&&&))

type CReader = Reader Constraints

--type CWriter out ret = Writer (Set out) ret

-- |A function modeling immediate compatibility.  This function takes a type and
--  a guard in a match case.  If the input type is compatible with the guard,
--  this function returns @CompatibleAs t@, where t is the type as which the
--  original type is compatible; otherwise, @NotCompatible@ is
--  returned. MaybeCompatible is returned if the result is not yet determinable,
--  as in the case of lazy operations not yet being closed over.  This function
--  is equivalent to the _ <:: _ ~ _ relation in the documentation.

immediatelyCompatible :: TauDown
                      -> TauChi
                      -> CReader (Maybe (TauDown, InterAlphaChain))
immediatelyCompatible tau chi =
  case (tau,chi) of
    (_,ChiAny) -> rJust tau
    (TdPrim p, ChiPrim p') | p == p' -> rJust tau
    (TdLabel n _, ChiLabel n' _) | n == n' -> rJust tau
    (TdOnionSub a s, _) | not $ tSubMatch s chi -> do
      ts <- concretizeType a
      rFilter $ Set.toList ts
    (TdOnion a1 a2, _) -> do
      t1s <- concretizeType a1
      t2s <- concretizeType a2
      rFilter $ Set.toList t2s ++ Set.toList t1s
    (TdFunc _, ChiFun) -> rJust tau
    _ -> return $ Nothing
    where rJust = return . Just . (id &&& IATerm)
          rFilter xs =
            listToMaybe . catMaybes <$> mapM helper xs
          helper (t, _) =
            immediatelyCompatible t chi

tSubMatch :: SubTerm -> TauChi -> Bool
tSubMatch subTerm chi =
  case (chi, subTerm) of
    (ChiPrim p, SubPrim p') -> p == p'
    (ChiLabel n _, SubLabel n') -> n == n'
    (ChiFun, SubFunc) -> True
    _ -> False

-- |A function modeling TCaseBind.  This function creates an appropriate set of
--  constraints to add when a given case branch is taken.  Its primary purpose
--  is to bind a label variable (such as `A x) to the contents of the input.
tCaseBind :: ConstraintHistory
          -> TauDown
          -> TauChi
          -> Constraints
tCaseBind history tau chi =
    case (tau,chi) of
        (TdLabel n tau', ChiLabel n' a) ->
            (tau' <: a .: history)
                `singIf` (n == n')
        _ -> Set.empty

--TODO: Consider adding chains to history and handling them here
--TODO: Docstring this function

-- This is particularly similar to an abstract class in the java sense
class (Eq a, Ord (LowerBound a), Ord (Chain a)) => LowerBounded a where
  type LowerBound a
  type Chain a

  concretizeType :: a -> CReader (Set (LowerBound a, Chain a))
  concretizeType a = do
    -- clbs are the concrete lower bounds and chains
    clbs <- concreteLowerBounds a
    ilbs <- intermediateLowerBounds a
    rec <- Set.unions <$> mapM concretizeIlb ilbs
    return $ Set.union (Set.fromList clbs) rec

  concretizeIlb :: (a, Constraint) -> CReader (Set (LowerBound a, Chain a))
  concretizeIlb (ilb, c) =
    concretizeType ilb >>= return . Set.map (second $ extendChain ilb c)

  concreteLowerBounds :: a -> CReader [(LowerBound a, Chain a)]
  concreteLowerBounds a = do
    cs <- ask
    return $ do
      (lb, a', c) <- findCLowerBounds cs
      guard $ a == a'
      return (lb, mkChain a c lb)

  intermediateLowerBounds :: a -> CReader [(a, Constraint)]
  intermediateLowerBounds a = do
    cs <- ask
    return $ do
      (ret, a', c) <- findILowerBounds cs
      guard $ a == a'
      return (ret, c)

  findCLowerBounds :: Constraints -> [(LowerBound a, a, Constraint)]
  findILowerBounds :: Constraints -> [(a, a, Constraint)]
  mkChain :: a -> Constraint -> LowerBound a -> Chain a
  extendChain :: a -> Constraint -> Chain a -> Chain a

instance LowerBounded InterAlpha where
  type LowerBound InterAlpha = TauDown
  type Chain InterAlpha = InterAlphaChain

  findCLowerBounds cs = do
    c@(LowerSubtype td a' _) <- Set.toAscList cs
    return (td, a', c)

  findILowerBounds cs = do
    c@(AlphaSubtype ret a' _) <- Set.toAscList cs
    return (ret, a', c)

  mkChain a c = IALink a c . IATerm
  extendChain = IALink

instance LowerBounded CellAlpha where
  type LowerBound CellAlpha = InterAlpha
  type Chain CellAlpha = CellAlphaChain

  findCLowerBounds cs = do
    c@(CellSubtype td a' _) <- Set.toAscList cs
    return (td, a', c)

  findILowerBounds cs = do
    c@(CellAlphaSubtype ret a' _) <- Set.toAscList cs
    return (ret, a', c)

  mkChain a c = CALink a c . CATerm . Cell
  extendChain = CALink

-- concretizeInterAlpha :: InterAlpha -> CReader (Set TauDown)
-- concretizeInterAlpha a = do
--   clbs <- concreteLowerBounds
--   ilbs <- intermediateLowerBounds
--   rec <- Set.unions <$> mapM concretizeInterAlpha ilbs
--   return $ Set.union (Set.fromList clbs) rec
--   where concreteLowerBounds :: CReader [TauDown]
--         concreteLowerBounds = do
--           constraints <- ask
--           return $ do
--             LowerSubtype td a' _ <- Set.toAscList constraints
--             guard $ a == a'
--             return td
--         intermediateLowerBounds :: CReader [InterAlpha]
--         intermediateLowerBounds = do
--           constraints <- ask
--           return $ do
--             AlphaSubtype ret a' _ <- Set.toAscList constraints
--             guard $ a == a'
--             return ret

-- |A function which performs substitution on a set of constraints.  All
--  variables in the alpha set are replaced with corresponding versions that
--  have the specified alpha in their call sites list.
substituteVars :: Constraints -> ForallVars -> CellAlpha -> Constraints
substituteVars constraints forallVars replAlpha =
  runReader
    (substituteAlpha constraints)
    (replAlpha, forallVars)

ct :: (LowerBounded a) => Constraints -> a -> [(LowerBound a, Chain a)]
ct cs a = Set.toList $ runReader (concretizeType a) cs

--TODO: case rules use explicit case on Maybe; possibly clean up.
closeCases :: Constraints -> Constraints
closeCases cs = Set.unions $ do
  c@(Case a guards _) <- Set.toList cs
  (tau, _) <- ct cs a
  -- Handle contradictions elsewhere, both to improve readability and to be more
  -- like the document.
  Just ret <- return $ join $ listToMaybe $ do
    Guard tauChi cs' <- guards
    case runReader (immediatelyCompatible tau tauChi) cs of
      Nothing -> return $ Nothing
      Just (tau', chain') ->
        let hist = ClosureCase c chain' in
        return $ Just $ Set.union cs' $ tCaseBind hist tau' tauChi
  return ret

findCaseContradictions :: Constraints -> Constraints
findCaseContradictions cs = Set.fromList $ do
  c@(Case a guards _) <- Set.toList cs
  (tau, chain) <- ct cs a
  isCont <- return $ null $ do
    Guard tauChi _ <- guards
    case runReader (immediatelyCompatible tau tauChi) cs of
      Nothing -> mzero
      Just _ -> return ()
  guard isCont
  return $ Bottom $ ContradictionCase c chain

closeApplications :: Constraints -> Constraints
closeApplications cs = Set.unions $ do
  c@(UpperSubtype a t@(TuFunc ai' ao') _) <- Set.toList cs
  (TdFunc (PolyFuncData foralls ai ao cs'), funcChain) <- ct cs a
  (ca3, caChain) <- ct cs ai'
  let funcChain' = IAHead t c funcChain
      hist = ClosureApplication funcChain' caChain
      cs'' = Set.union cs' $
               Set.fromList [ Cell ca3 <: ai .: hist
                            , ao <: ao' .: hist ]
  return $ substituteVars cs'' foralls ai'

findNonFunctionApplications :: Constraints -> Constraints
findNonFunctionApplications cs = Set.fromList $ do
  c@(UpperSubtype a t@(TuFunc {}) _) <- Set.toList cs
  (tau, chain) <- ct cs a
  let chain' = IAHead t c chain
  case tau of
    TdFunc (PolyFuncData {}) -> mzero
    _ -> return $ Bottom $ ContradictionAppliction chain

closeLops :: Constraints -> Constraints
closeLops cs = Set.fromList $ do
-- TODO: assumes all lops are int -> int -> int
  c@(LazyOpSubtype _ a1 a2 a _) <- Set.toList cs
  (TdPrim PrimInt, chain1) <- ct cs a1
  (TdPrim PrimInt, chain2) <- ct cs a2
  return $ TdPrim PrimInt <: a .: ClosureLop c chain1 chain2

findLopContradictions :: Constraints -> Constraints
findLopContradictions cs = Set.fromList $ do
  c@(LazyOpSubtype _ a1 a2 _ _) <- Set.toList cs
  -- Not quite like the document.
  -- FIXME: when we have lops that aren't int -> int -> int, this needs to be
  -- changed.
  (tau, chain) <- ct cs a1 ++ ct cs a2
  case tau of
    TdPrim PrimInt -> mzero
    _ -> return $ Bottom $ ContradictionLop c chain

propogateCellsForward :: Constraints -> Constraints
propogateCellsForward cs = Set.fromList $ do
  c@(CellGetSubtype a a1 _) <- Set.toList cs
  (a2, a2Chain) <- ct cs a
  (t2, t2Chain) <- ct cs a2
  let a2Chain' = CAHeadG (CellGet a1) c a2Chain
  return $ t2 <: a1 .: ClosureCellForward a2Chain' t2Chain

propogateCellsBackward :: Constraints -> Constraints
propogateCellsBackward cs = Set.fromList $ do
  c@(CellSetSubtype a a1 _) <- Set.toList cs
  (a2, a2Chain) <- ct cs a
  (t2, t2Chain) <- ct cs a1
  let a2Chain' = CAHeadS (CellSet a1) c a2Chain
  return $ t2 <: a2 .: ClosureCellBackward a2Chain t2Chain

-- |This closure calculation function produces appropriate bottom values for
--  immediate contradictions (such as tprim <: tprim' where tprim != tprim').
closeSingleContradictions :: Constraints -> Constraints
closeSingleContradictions cs =
  Set.unions $ map ($ cs)
    [ id
    , findCaseContradictions
    , findNonFunctionApplications
    , findLopContradictions
    ]

closeAll :: Constraints -> Constraints
closeAll cs =
  Set.unions $ map ($ cs)
    [ id
    , closeCases
    , closeApplications
    , closeLops
    , propogateCellsForward
    , propogateCellsBackward
    ]

-- |Calculates the transitive closure of a set of type constraints.
calculateClosure :: (?debug :: Bool) => Constraints -> Constraints
calculateClosure c = ddisp $ closeSingleContradictions $ leastFixedPoint closeAll $ ddisp $ c
  where ddisp x =
          if ?debug
            then trace ("{-----\n" ++ display x ++ "\n-----}") x
            else x

saHelper :: (AlphaSubstitutable a)
         => (a -> b) -> a -> Reader AlphaSubstitutionEnv b
saHelper constr a = constr <$> substituteAlpha a

saHelper2 :: (AlphaSubstitutable a1, AlphaSubstitutable a2)
          => (a1 -> a2 -> b) -> a1 -> a2 -> Reader AlphaSubstitutionEnv b
saHelper2 constr a1 a2 = constr <$> substituteAlpha a1 <*> substituteAlpha a2

instance AlphaSubstitutable TauUp where
  substituteAlpha tu = case tu of
    TuFunc a1 a2 -> saHelper2 TuFunc a1 a2

instance AlphaSubstitutable TauDown where
  substituteAlpha td = case td of
    TdLabel n a -> saHelper (TdLabel n) a
    TdOnion a1 a2 -> saHelper2 TdOnion a1 a2
    TdFunc pfd -> saHelper TdFunc pfd
    TdOnionSub a s -> saHelper (`TdOnionSub` s) a
    _ -> return td

instance AlphaSubstitutable PolyFuncData where
  substituteAlpha (PolyFuncData alphas alphaIn alphaOut constraints) =
      PolyFuncData alphas
        <$> substituteAlpha' alphaIn
        <*> substituteAlpha' alphaOut
        <*> substituteAlpha' constraints
      -- The variables described by the forall list should never be replaced
      where substituteAlpha' :: (AlphaSubstitutable a)
                             => a -> Reader AlphaSubstitutionEnv a
            substituteAlpha' =
              local (second $ flip Set.difference alphas) . substituteAlpha



csaHelper :: (AlphaSubstitutable a)
          => (a -> hist -> b)
          -> a -> hist -> Reader AlphaSubstitutionEnv b
csaHelper constr a hist =
  constr <$> substituteAlpha a <*> pure hist

csaHelper2 :: (AlphaSubstitutable a1, AlphaSubstitutable a2)
           => (a1 -> a2 -> hist -> b)
           -> a1 -> a2 -> hist -> Reader AlphaSubstitutionEnv b
csaHelper2 constr a1 a2 hist =
  constr <$> substituteAlpha a1 <*> substituteAlpha a2 <*> pure hist

csaHelper3 :: (AlphaSubstitutable a1,
               AlphaSubstitutable a2,
               AlphaSubstitutable a3)
           => (a1 -> a2 -> a3 -> hist -> b)
           -> a1 -> a2 -> a3 ->  hist -> Reader AlphaSubstitutionEnv b
csaHelper3 constr a1 a2 a3 hist =
  constr
    <$> substituteAlpha a1
    <*> substituteAlpha a2
    <*> substituteAlpha a3
    <*> pure hist

-- |A typeclass for entities which can substitute their type variables.
class AlphaSubstitutable a where
  -- |The alpha in the reader environment is added to superscripts.
  --  The set in the reader environment contains alphas to ignore.
  substituteAlpha :: a -> Reader AlphaSubstitutionEnv a

instance (AlphaType a) => AlphaSubstitutable (SomeAlpha a) where
  substituteAlpha = substituteAlphaHelper

instance AlphaSubstitutable AnyAlpha where
  substituteAlpha = substituteAlphaHelper

instance AlphaSubstitutable Constraint where
  substituteAlpha c = case c of
      LowerSubtype td a hist ->
        csaHelper2 LowerSubtype td a hist
      UpperSubtype a tu hist ->
        csaHelper2 UpperSubtype a tu hist
      AlphaSubtype a1 a2 hist ->
        csaHelper2 AlphaSubtype a1 a2 hist
      CellSubtype ia ca hist ->
        csaHelper2 CellSubtype ia ca hist
      CellGetSubtype ca ia hist ->
        csaHelper2 CellGetSubtype ca ia hist
      CellSetSubtype ca ia hist ->
        csaHelper2 CellSetSubtype ca ia hist
      CellAlphaSubtype a1 a2 hist ->
        csaHelper2 CellAlphaSubtype a1 a2 hist
      LazyOpSubtype op a1 a2 a3 hist ->
        csaHelper3 (LazyOpSubtype op) a1 a2 a3 hist
      Comparable a1 a2 hist ->
        csaHelper2 Comparable a1 a2 hist
      Case a guards hist ->
        Case
          <$> substituteAlpha a
          <*> mapM substituteAlpha guards
          <*> return hist
      Bottom hist -> return $ Bottom hist

instance AlphaSubstitutable Guard where
  substituteAlpha (Guard tauChi constraints) =
      Guard
        <$> substituteAlpha tauChi
        <*> substituteAlpha constraints

instance AlphaSubstitutable TauChi where
  substituteAlpha c = case c of
      ChiPrim p -> return $ ChiPrim p
      ChiLabel n a -> ChiLabel n <$> substituteAlpha a
      ChiFun -> return ChiFun
      ChiAny -> return ChiAny

instance (Ord a, AlphaSubstitutable a) => AlphaSubstitutable (Set a) where
  substituteAlpha = fmap Set.fromList . mapM substituteAlpha . Set.toList
