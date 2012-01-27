{-# LANGUAGE TupleSections, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
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
                                     , Sigma(..)
                                     , ForallVars
                                     , Cell(..)
                                     )

import Language.TinyBang.Types.Alphas ( InterAlpha
                                      , CellAlpha
                                      , SomeAlpha
                                      , InterType
                                      , CellType
                                      , AlphaType
                                      , AnyAlpha
                                      , AlphaSubstitutionEnv
                                      , substituteAlphaHelper
                                      )

import Data.Function.Utils (leastFixedPoint)
import Data.Set.Utils (singIf)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, catMaybes)
import Control.Monad.Reader (runReader, ask, local, Reader, MonadReader)
import Control.Monad (guard, join, mzero)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (second)

type CReader = Reader Constraints

--type CWriter out ret = Writer (Set out) ret

data Compatibility = NotCompatible | MaybeCompatible | CompatibleAs TauDown

histFIXME :: ConstraintHistory
histFIXME = IDontCare

-- |A function modeling immediate compatibility.  This function takes a type and
--  a guard in a match case.  If the input type is compatible with the guard,
--  this function returns @CompatibleAs t@, where t is the type as which the
--  original type is compatible; otherwise, @NotCompatible@ is
--  returned. MaybeCompatible is returned if the result is not yet determinable,
--  as in the case of lazy operations not yet being closed over.  This function
--  is equivalent to the _ <:: _ ~ _ relation in the documentation.

immediatelyCompatible :: TauDown
                      -> TauChi
                      -> CReader (Maybe TauDown)
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
    where rJust = return . Just
          rFilter xs =
            listToMaybe . catMaybes <$> mapM (`immediatelyCompatible` chi) xs

tSubMatch :: Sigma -> TauChi -> Bool
tSubMatch sigma chi =
  case (chi, sigma) of
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
class (Eq a, Ord (LowerBound a)) => LowerBounded a where
  type LowerBound a

  concretizeType :: a -> CReader (Set (LowerBound a))
  concretizeType a = do
    clbs <- concreteLowerBounds a
    ilbs <- intermediateLowerBounds a
    rec <- Set.unions <$> mapM concretizeType ilbs
    return $ Set.union (Set.fromList clbs) rec

  concreteLowerBounds :: a -> CReader [LowerBound a]
  concreteLowerBounds a = do
    cs <- ask
    return $ do
      (lb, a') <- findCLowerBounds cs
      guard $ a == a'
      return lb

  intermediateLowerBounds :: a -> CReader [a]
  intermediateLowerBounds a = do
    cs <- ask
    return $ do
      (ret, a') <- findILowerBounds cs
      guard $ a == a'
      return ret

  findCLowerBounds :: Constraints -> [(LowerBound a, a)]
  findILowerBounds :: Constraints -> [(a, a)]

instance LowerBounded (SomeAlpha InterType) where
  type LowerBound (SomeAlpha InterType)= TauDown

  findCLowerBounds cs = do
    LowerSubtype td a' _ <- Set.toAscList cs
    return (td, a')

  findILowerBounds cs = do
    AlphaSubtype ret a' _ <- Set.toAscList cs
    return (ret, a')

instance LowerBounded (SomeAlpha CellType) where
  type LowerBound (SomeAlpha CellType) = InterAlpha

  findCLowerBounds cs = do
    CellSubtype td a' _ <- Set.toAscList cs
    return (td, a')

  findILowerBounds cs = do
    CellAlphaSubtype ret a' _ <- Set.toAscList cs
    return (ret, a')

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

ct cs a = Set.toList $ runReader (concretizeType a) cs

closeCases :: Constraints -> Constraints
closeCases cs = Set.unions $ do
  Case a guards _ <- Set.toList cs
  tau <- ct cs a
  -- Handle contradictions elsewhere, both to improve readability and to be more
  -- like the document.
  Just ret <- return $ join $ listToMaybe $ do
    Guard tauChi cs' <- guards
    case runReader (immediatelyCompatible tau tauChi) cs of
      Nothing -> return $ Nothing
      Just tau' ->
        return $ Just $ Set.union cs' $ tCaseBind histFIXME tau' tauChi
  return ret

findCaseContradictions :: Constraints -> Constraints
findCaseContradictions cs = Set.fromList $ do
  Case a guards _ <- Set.toList cs
  tau <- ct cs a
  isCont <- return $ null $ do
    Guard tauChi _ <- guards
    case runReader (immediatelyCompatible tau tauChi) cs of
      Nothing -> mzero
      Just _ -> return ()
  guard isCont
  return $ Bottom histFIXME

closeApplications :: Constraints -> Constraints
closeApplications cs = Set.unions $ do
  UpperSubtype a (TuFunc ai' ao') _ <- Set.toList cs
  TdFunc (PolyFuncData foralls ai ao cs') <- ct cs a
  ca3 <- ct cs ai'
  let cs'' = Set.union cs' $
               Set.fromList [ Cell ca3 <: ai .: histFIXME
                            , ao <: ao' .: histFIXME]
  return $ substituteVars cs'' foralls ai'

findNonFunctionApplications :: Constraints -> Constraints
findNonFunctionApplications cs = Set.fromList $ do
  UpperSubtype a (TuFunc {}) _ <- Set.toList cs
  tau <- ct cs a
  case tau of
    TdFunc (PolyFuncData {}) -> mzero
    _ -> return $ Bottom histFIXME

closeLops :: Constraints -> Constraints
closeLops cs = Set.fromList $ do
-- TODO: assumes all lops are int -> int -> int
  LazyOpSubtype _ a1 a2 a _ <- Set.toList cs
  TdPrim PrimInt <- ct cs a1
  TdPrim PrimInt <- ct cs a2
  return $ TdPrim PrimInt <: a .: histFIXME

findLopContradictions :: Constraints -> Constraints
findLopContradictions cs = Set.fromList $ do
  LazyOpSubtype _ a1 a2 _ _ <- Set.toList cs
  -- Not quite like the document.
  -- FIXME: when we have lops that aren't int -> int -> int, this needs to be
  -- changed.
  tau <- ct cs a1 ++ ct cs a2
  case tau of
    TdPrim PrimInt -> mzero
    _ -> return $ Bottom histFIXME

propogateCellsForward :: Constraints -> Constraints
propogateCellsForward cs = Set.fromList $ do
  CellGetSubtype a a1 _ <- Set.toList cs
  a2 <- ct cs a
  t2 <- ct cs a2
  return $ t2 <: a1 .: histFIXME

propogateCellsBackward :: Constraints -> Constraints
propogateCellsBackward cs = Set.fromList $ do
  CellSetSubtype a a1 _ <- Set.toList cs
  a2 <- ct cs a
  t2 <- ct cs a1
  return $ t2 <: a2 .: histFIXME

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
calculateClosure :: Constraints -> Constraints
calculateClosure c = closeSingleContradictions $ leastFixedPoint closeAll c

saHelper constr a = constr <$> substituteAlpha a
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

csaHelper constr a1 a2 hist =
  constr <$> substituteAlpha a1 <*> substituteAlpha a2 <*> pure hist

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
      LowerSubtype td a hist -> do
        a' <- substituteAlpha a
        return $ LowerSubtype td a' hist
      UpperSubtype a tu hist -> do
        a' <- substituteAlpha a
        return $ UpperSubtype a' tu hist
      AlphaSubtype a1 a2 hist ->
        csaHelper AlphaSubtype a1 a2 hist
      CellSubtype ia ca hist ->
        csaHelper CellSubtype ia ca hist
      CellGetSubtype ca ia hist ->
        csaHelper CellGetSubtype ca ia hist
      CellSetSubtype ca ia hist ->
        csaHelper CellSetSubtype ca ia hist
      CellAlphaSubtype a1 a2 hist ->
        csaHelper CellAlphaSubtype a1 a2 hist
      LazyOpSubtype op a1 a2 a3 hist ->
        csaHelper3 (LazyOpSubtype op) a1 a2 a3 hist
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
