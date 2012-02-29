{-# LANGUAGE TupleSections
           , TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , ImplicitParams
           , TypeSynonymInstances
           , GADTs
           , ScopedTypeVariables #-}
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
                                     , TauProj(..)
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
                                     , histFIXME
                                     )

import Data.Function.Utils (leastFixedPoint)

import Debug.Trace (trace)
import Utils.Render.Display (display, Display)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Maybe (listToMaybe, isNothing, mapMaybe)
import Control.Monad.Reader (runReader, ask, local, Reader, MonadReader)
import Control.Monad (guard, mzero, liftM)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (second)

type CReader = Reader Constraints

--type CWriter out ret = Writer (Set out) ret

-- |A function modeling pattern compatibility.  This function takes a type and
--  a guard in a match case.  This function returns a list of the constraint
--  sets under which the input type is compatible with the guard.  If the type
--  is incompatible, an empty list is returned.
patternCompatible :: TauDown -> TauChi a
                  -> CReader [Constraints]
-- TODO: a compatibility history should be returned as well to retain the proof
patternCompatible tau chi =
  case chi of
    TauChiTopVar a -> return $ [Set.singleton (tau <: a .: histFIXME )]
    TauChiTopOnion p s -> do -- Maybe
      c1s <- patternCompatible tau p
      c2s <- patternCompatible tau s
      return [Set.union c1 c2 | c1 <- c1s, c2 <- c2s]
    TauChiTopBind b -> patternCompatible tau b
    TauChiOnionMany p s -> do -- Maybe
      c1s <- patternCompatible tau p
      c2s <- patternCompatible tau s
      return [Set.union c1 c2 | c1 <- c1s, c2 <- c2s]
    TauChiOnionOne p -> patternCompatible tau p
    TauChiBound a b -> do -- Maybe
      c <- patternCompatible tau b
      return $ map (Set.insert (tau <: a .: histFIXME)) c
    TauChiUnbound p -> patternCompatible tau p
    TauChiPrim p -> destructOnOnion $
      \t -> return $
        case t of
          TdPrim p' | p == p' -> [Set.empty]
          _ -> []
    TauChiLabelShallow n a2 -> destructOnOnion $
      \t -> return $
        case t of
          TdLabel n' a1 | n == n' -> [Set.singleton (a1 <: a2 .: histFIXME)]
          _ -> []
    TauChiLabelDeep n b -> destructOnOnion $
      \t ->
        case t of
          TdLabel n' ac | n == n' -> do -- CReader
            aihs <- concretizeType ac
            -- TODO: care about history!
            let ais = Set.toList $ Set.map fst aihs
            concretizations <- mapM concretizeType ais
            let taus = concatMap (Set.toList . Set.map fst) concretizations
            concat <$> mapM (flip patternCompatible b) taus
          _ -> return []
    TauChiFun -> destructOnOnion $
      \t -> return $
        case t of
          TdFunc _ -> [Set.empty]
          _ -> []
    TauChiInnerStruct s -> patternCompatible tau s
  where destructOnOnion :: (TauDown -> CReader [Constraints])
                        -> CReader [Constraints]
        destructOnOnion handler =
          case tau of
            TdOnion a1 a2 -> do -- CReader
              t1hs <- concretizeType a1
              t2hs <- concretizeType a2
              -- TODO: care about history!
              let t1s = Set.toList $ Set.map fst t1hs
              let t2s = Set.toList $ Set.map fst t2hs
              -- For each compatible t2, we have a set of results
              -- For any incompatible t2, we use results from the
              -- t1s.  The following are of type [[Constraints]]
              -- where the presence of [] in the outer list shows
              -- an incompatibility.
              t1compats <- mapM (flip patternCompatible chi) t1s
              t2compats <- mapM (flip patternCompatible chi) t2s
              return $ concat $ t2compats ++
                (if elem [] t2compats then t1compats else [])
            TdOnionSub a s ->
              -- Produce a result if and only if we don't match the onion
              -- subtraction term.
              if tSubMatch s chi
                then return []
                else
                  do -- CReader
                    ths <- concretizeType a
                    -- TODO: care about history!
                    let ts = Set.toList $ Set.map fst ths
                    (liftM concat) $ mapM (flip patternCompatible chi) ts
            _ -> handler tau

tSubMatch :: SubTerm -> TauChi a -> Bool
tSubMatch subTerm chi =
  case (chi, subTerm) of
    (TauChiPrim p, SubPrim p') -> p == p'
    (TauChiLabelShallow n _, SubLabel n') -> n == n'
    (TauChiLabelDeep n _, SubLabel n') -> n == n'
    (TauChiFun, SubFunc) -> True
    _ -> False

-- |Represents the type projection function from the documentation.  This
--  function determines the types which can be projected from a given type
--  using a specific projector.  There may be many such types; for instance,
--  an onion of a1 & a2 \ { `A int <: a1, `A unit <: a2, int <: a2 } might
--  project either `A int or `A unit for the projector `A (depending on the
--  flow taken).  An empty list indicates that no projection is legal.
tProj :: TauDown -> TauProj -> CReader [TauDown]
tProj tau tproj =
  case (tau, tproj) of
    (TdPrim p, TpPrim p') | p == p' -> return [tau]
    (TdLabel n _, TpLabel n') | n == n' -> return [tau]
    (TdOnion a1 a2, _) -> do -- CReader
      t1hs <- concretizeType a1
      t2hs <- concretizeType a2
      -- TODO: care about history!
      let t1s = Set.toList $ Set.map fst t1hs
      let t2s = Set.toList $ Set.map fst t2hs
      t1projs <- mapM (flip tProj tproj) t1s
      t2projs <- mapM (flip tProj tproj) t2s
      return $ concat $ t2projs ++
        (if elem [] t2projs then t1projs else [])
    (TdOnionSub a s, _) ->
      if tSubProj s tproj
        then return []
        else do -- CReader
          ths <- concretizeType a
          -- TODO: care about history
          let ts = Set.toList $ Set.map fst ths
          concat <$> mapM (flip tProj tproj) ts
    (TdFunc _, TpFun) -> return [tau]
    _ -> return []

-- Performs a check to ensure that projection can occur through an onion
-- subtraction type.
tSubProj :: SubTerm -> TauProj -> Bool
tSubProj s tproj =
  case (tproj,s) of
    (TpPrim p, SubPrim p') -> p == p'
    (TpLabel n, SubLabel n') -> n == n'
    (TpFun, SubFunc) -> True
    _ -> False

--TODO: Consider adding chains to history and handling them here
--TODO: Docstring this function

-- This is particularly similar to an abstract class in the java sense
class ( Display a
      , Eq a
      , Ord a
      , Ord (LowerBound a)
      , Ord (Chain a)
      , Display (Chain a))
      => LowerBounded a where
  type LowerBound a
  type Chain a

  concretizeType :: a -> CReader (Set (LowerBound a, Chain a))
  concretizeType = concretizeType' Set.empty
  
  concretizeType' :: Set a -> a -> CReader (Set (LowerBound a, Chain a))
  concretizeType' visited a =
    if Set.member a visited
      then return Set.empty
      else do
        -- clbs are the concrete lower bounds and chains
        clbs <- concreteLowerBounds a
        ilbs <- intermediateLowerBounds a
        rec <- Set.unions <$> mapM (concretizeIlb (Set.insert a visited)) ilbs
        return $ Set.union (Set.fromList clbs) rec

  concretizeIlb :: Set a -> (a, a, Constraint)
                -> CReader (Set (LowerBound a, Chain a))
  concretizeIlb visited (la, ua, c) =
    concretizeType' visited la >>= return . Set.map (second $ extendChain ua c)

  concreteLowerBounds :: a -> CReader [(LowerBound a, Chain a)]
  concreteLowerBounds a = do
    cs <- ask
    return $ do
      (lb, a', c) <- findCLowerBounds cs
      guard $ a == a'
      return (lb, mkChain a c lb)

  intermediateLowerBounds :: a -> CReader [(a, a, Constraint)]
  intermediateLowerBounds a = do
    cs <- ask
    return $ do
      (ret, a', c) <- findILowerBounds cs
      guard $ a == a'
      return (ret, a, c)

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
--  have the specified alpha in their call sites list.  This function
--  corresponds to the Application Substitution Definition in the language
--  specification.
substituteVars :: Constraints -> ForallVars -> CellAlpha -> Constraints
substituteVars constraints forallVars replAlpha =
  runReader
    (substituteAlpha constraints)
    (replAlpha, forallVars)

-- |Performs cycle detection on a set of constraints.
cycleDetectGeneric :: forall a. (AlphaType a)
                   => (Constraint -> Maybe (SomeAlpha a, SomeAlpha a))
                        -- ^A function used to extract appropriate subtyping
                        --  information from a constraint.  If the constraint
                        --  represents a subtype of the appropriate variables,
                        --  it returns Just that pair of variables with the
                        --  subtype first.  Otherwise, it returns Nothing.
                   -> Constraints
                        -- ^The set of constraints to check for cycles.
                   -> Set (Set (SomeAlpha a))
                        -- ^A set of resulting cycles; each cycle is a set of
                        --  type variables which can be equivocated.
cycleDetectGeneric f cs =
  -- Start by finding all appropriate constraints and creating the
  -- cycle detecting triple from them.  The triple elements are a set of
  -- visited type variables, the lower bounding type variable, and the
  -- upper bounding type variable.  The set always contains the current
  -- lower bounding type variable.
  let initial = Set.fromList $ map (tupleToTriple Set.empty) $ map swap pairs in
  let result = leastFixedPoint step $ initial in
  Set.map (\(s,_,_) -> s) $ Set.filter (\(_,a,b) -> a == b) result
  where step :: (AlphaType a)
             => Set (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
             -> Set (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
        step s = Set.unions $ map onestep $ Set.toList s
        onestep :: (AlphaType a)
                => (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
                -> Set (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
        onestep x@(s,a,b) =
          if a == b
            then Set.singleton x
            else
              Set.insert x $ Set.fromList $ map (tupleToTriple s) $
                do -- List
                  (a',b') <- pairs
                  guard $ a == a'
                  return (b',b)
        tupleToTriple :: (AlphaType a)
                      => Set (SomeAlpha a)
                      -> (SomeAlpha a, SomeAlpha a)
                      -> (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
        tupleToTriple s (a,b) = (Set.insert a s, a, b)
        pairs :: [(SomeAlpha a, SomeAlpha a)]
        pairs = mapMaybe f $ Set.toList cs

cycleDetectInter :: Constraints -> Set (Set InterAlpha)
cycleDetectInter = cycleDetectGeneric $ \c ->
  case c of
    AlphaSubtype a b _ -> Just (a,b)
    _ -> Nothing

cycleDetectCell :: Constraints -> Set (Set CellAlpha)
cycleDetectCell = cycleDetectGeneric $ \c ->
  case c of
    CellAlphaSubtype a b _ -> Just (a,b)
    _ -> Nothing

ct :: (LowerBounded a) => Constraints -> a -> [(LowerBound a, Chain a)]
ct cs a = Set.toList $ runReader (concretizeType a) cs

closeCases :: Constraints -> Constraints
closeCases cs = Set.unions $ do
  c@(Case a guards _) <- Set.toList cs
  (tau, _) <- ct cs a
  -- Given a concretization, the first guard which matches is the one that
  -- applies.  We'll build a list of the matches and filter on it.
  return $ maybe Set.empty Set.unions $ caseGuardResults cs guards tau

findCaseContradictions :: Constraints -> Constraints
findCaseContradictions cs = Set.fromList $ do
  c@(Case a guards _) <- Set.toList cs
  (tau, chain) <- ct cs a
  guard $ isNothing $ caseGuardResults cs guards tau
  return $ Bottom $ ContradictionCase c chain

-- |A utility function used in case analysis.  Given a reader constraint set,
--  a list of guards, and a type, this function produces Just a list of
--  constraints (if the type matched one of the guards) or Nothing if none of
--  the guards matched.
caseGuardResults :: Constraints -> [Guard] -> TauDown -> Maybe [Constraints]
caseGuardResults cs guards tau =
  listToMaybe $ filter (not . null) $ map check guards
  where check :: Guard -> [Constraints]
        check (Guard tauChi cs') =
          map (Set.union cs') $
            runReader (patternCompatible tau tauChi) cs

closeApplications :: Constraints -> Constraints
closeApplications cs = Set.unions $ do -- List
  c@(UpperSubtype a t@(TuFunc ai' ao') _) <- Set.toList cs
  (td, funcChain) <- ct cs a
  TdFunc (PolyFuncData foralls ai ao cs') <- runReader (tProj td TpFun) cs
  (ca3, caChain) <- ct cs ai'
  let funcChain' = IAHead t c funcChain
      hist = ClosureApplication funcChain' caChain
      cs'' = Set.union cs' $
               Set.fromList [ Cell ca3 <: ai .: hist
                            , ao <: ao' .: hist ]
  return $ substituteVars cs'' foralls ai'

findNonFunctionApplications :: Constraints -> Constraints
findNonFunctionApplications cs = Set.fromList $ do -- List
  c@(UpperSubtype a t@(TuFunc {}) _) <- Set.toList cs
  (tau, chain) <- ct cs a
  let chain' = IAHead t c chain
  guard $ null $ runReader (tProj tau TpFun) cs
  return $ Bottom $ ContradictionApplication chain'

closeLops :: Constraints -> Constraints
closeLops cs = Set.fromList $ do
  c@(LazyOpSubtype _ a1 a2 a _) <- Set.toList cs
  (td1, chain1) <- ct cs a1
  (td2, chain2) <- ct cs a2
  -- NOTE: assumes that all lops are int -> int -> int
  guard $ not . null $ runReader (tProj td1 $ TpPrim PrimInt) cs
  guard $ not . null $ runReader (tProj td2 $ TpPrim PrimInt) cs
  return $ TdPrim PrimInt <: a .: ClosureLop c chain1 chain2

findLopContradictions :: Constraints -> Constraints
findLopContradictions cs = Set.fromList $ do
  c@(LazyOpSubtype _ a1 a2 _ _) <- Set.toList cs
  -- NOTE: assumes that all lops are int -> int -> int
  (td, chain) <- ct cs a1 ++ ct cs a2
  if null $ runReader (tProj td $ TpPrim PrimInt) cs
    then return $ Bottom $ ContradictionLop c chain
    else mzero

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
  return $ t2 <: a2 .: ClosureCellBackward a2Chain' t2Chain

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

instance AlphaSubstitutable (TauChi a) where
  substituteAlpha c =
    case c of
      TauChiTopVar a -> TauChiTopVar <$> substituteAlpha a
      TauChiTopOnion s p ->
        TauChiTopOnion <$> substituteAlpha s <*> substituteAlpha p
      TauChiTopBind b -> TauChiTopBind <$> substituteAlpha b
      TauChiOnionMany s p ->
        TauChiOnionMany <$> substituteAlpha s <*> substituteAlpha p
      TauChiOnionOne p -> TauChiOnionOne <$> substituteAlpha p
      TauChiBound a b ->
        TauChiBound <$> substituteAlpha a <*> substituteAlpha b
      TauChiUnbound p -> TauChiUnbound <$> substituteAlpha p
      TauChiPrim _ -> return c
      TauChiLabelShallow n a -> TauChiLabelShallow n <$> substituteAlpha a
      TauChiLabelDeep n b -> TauChiLabelDeep n <$> substituteAlpha b
      TauChiFun -> return c
      TauChiInnerStruct s -> TauChiInnerStruct <$> substituteAlpha s

instance (Ord a, AlphaSubstitutable a) => AlphaSubstitutable (Set a) where
  substituteAlpha = fmap Set.fromList . mapM substituteAlpha . Set.toList
