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

import qualified Language.TinyBang.Config as Cfg
import Language.TinyBang.Types.Types ( (<:)
                                     , (.:)
                                     , Constraints
                                     , Constraint(..)
                                     , TauDown(..)
                                     , PatternType(..)
                                     , PrimaryPatternType(..)
                                     , ConstraintHistory(..)
                                     , ScapeData(..)
                                     , PrimitiveType(..)
                                     , ProjTerm(..)
                                     , ForallVars(..)
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
                                     , Alpha(..)
                                     , Contour
                                     , contour
                                     , unContour
                                     , histFIXME
                                     )

import Data.Function.Utils (leastFixedPoint)

import Debug.Trace (trace)
import Utils.Render.Display

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (listToMaybe, isNothing, catMaybes)
import Control.Monad.Reader (runReader, ask, local, Reader, MonadReader)
import Control.Monad.RWS (runRWS, RWS)
import Control.Monad (guard, mzero)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (first, second)
import Control.Exception (assert)

type CReader = Reader Constraints

--type CWriter out ret = Writer (Set out) ret

-- |A function modeling pattern compatibility.  This function takes a
--  type and a guard in a match case.  This function returns a list of
--  the maybe constraint sets under which the input type is compatible
--  with the guard, where a Nothing indicates incompatibility.
patternCompatible :: TauDown -> PatternType -> CReader [Maybe Constraints]
-- TODO: a compatibility history should be returned as well to retain the proof
patternCompatible tau (Pattern a pp) = do
  map (fmap $ Set.insert $ tau <: a .: histFIXME)
    <$> primaryPatternCompatible tau pp

primaryPatternCompatible :: TauDown -> PrimaryPatternType
                         -> CReader [Maybe Constraints]
-- TODO: a compatibility history should be returned as well to retain the proof
primaryPatternCompatible tau tpat =
  case tpat of
    PatPrim prim ->
      checkProjections (ProjPrim prim) $
        \projection -> if null projection then failure else easySuccess
    PatLabel lbl a2 pp ->
      checkProjections (ProjLabel lbl) $
        \projection ->
          case listToMaybe [a1 | TdLabel _ a1 <- projection] of
            Just a1 -> do -- CReader
              taus <- concretizeCellVariable a1
              -- The fst is what throws out history
              css <- mapM ((`primaryPatternCompatible` pp) . fst) taus
              return $ do -- List
                cs <- css
                mc <- cs
                return $ Set.insert (a1 <: a2 .: histFIXME) <$> mc
            -- An empty list is returned only in the case where there are no
            -- suitable projections.
            Nothing -> failure
    PatOnion xs -> do
      xs' <- mapM (primaryPatternCompatible tau) xs
      -- Since xs' is a list of lists of constraints sets, where at least one
      -- element in each inner list must be satisfied, we need to do a
      -- permutation, which is what sequence does on lists, and then union.

      -- Since each element of sequence xs' is a [Maybe Constraints], and if any
      -- of these is a Nothing, the result should be a Nothing; otherwise, the
      -- constraints should be unioned, hence the parenthesized expression
      -- below.
      return $ map (fmap Set.unions . sequence) $ sequence xs'
    PatFun ->
      checkProjections ProjFunc $
        \projection -> if null projection then failure else easySuccess
  where easySuccess :: CReader [Maybe Constraints]
        easySuccess = return $ [Just Set.empty]
        failure :: CReader [Maybe Constraints]
        failure = return $ [Nothing]
        checkProjections :: ProjTerm
                         -> ([TauDown] -> CReader [Maybe Constraints])
                         -> CReader [Maybe Constraints]
        checkProjections proj handler = do -- CReader
          projections <- tProj tau proj
          concat <$> mapM handler projections

concretizeCellVariable :: CellAlpha
                       -> CReader ([( TauDown
                                    , (CellAlphaChain, InterAlphaChain))])
concretizeCellVariable ca = do
  intermediateConcretizations <- Set.toList <$> concretizeType ca
  sets <- sequence [ Set.map (f ic) <$> concretizeType ia
                   | (ia, ic) <- intermediateConcretizations]
  return $ concat $ map Set.toList sets
  where f x (y, z) = (y, (x, z))

-- tSubMatch :: ProjTerm -> PatternType -> Bool
-- tSubMatch subTerm pat =
--   case (pat, subTerm) of
--     (TauChiPrim p, ProjPrim p') -> p == p'
--     (TauChiLabelShallow n _, ProjLabel n') -> n == n'
--     (TauChiLabelDeep n _, ProjLabel n') -> n == n'
--     (TauChiFun, ProjFunc) -> True
--     _ -> False

-- |Represents the type projection relation from the documentation.  This
--  relation determines the types which can be projected from a given type
--  using a specific projector.  This function returns a list of possible
--  projection results (as projection is a relation); each result is a list of
--  the types which may have been projected.  For instance, projecting `A from
--  "a1 & a2 \ { `A int <: a1, `A unit <: a2, int <: a2 }" would result in
--  "[[`A int],[`A unit]]", as these represent the projections from each flow.
tProj :: TauDown -> ProjTerm -> CReader [[TauDown]]
tProj tau proj =
  case (tau, proj) of
    (TdPrim p, ProjPrim p') | p == p' -> return [[tau]]
    (TdLabel lbl _, ProjLabel lbl') | lbl == lbl' -> return [[tau]]
    (TdOnion a1 a2, _) -> do
      -- TODO: care about history; the map fst gets rid of it
      ctaus1 <- map fst <$> Set.toList <$> concretizeType a1
      ctaus2 <- map fst <$> Set.toList <$> concretizeType a2
      -- cprojss: all projections from every concretization of a variable
      cprojss1 <- mapM (flip tProj proj) ctaus1
      cprojss2 <- mapM (flip tProj proj) ctaus2
      -- We have all of the projections from a1 and all of the projections
      -- from a2.  Every combination of each one is valid.
      return $ do -- List
        -- cprojs: all projections from a specific concretization
        cprojs1 <- cprojss1
        cprojs2 <- cprojss2
        -- cproj: one projection from a specific concretization
        cproj1 <- cprojs1
        cproj2 <- cprojs2
        return $ cproj2 ++ cproj1
    (TdOnionSub a proj', _) | proj /= proj' -> projectionsFromVariable a
    (TdOnionProj a proj', _) | proj == proj' -> projectionsFromVariable a
    (TdScape _, ProjFunc) -> return [[tau]]
    _ -> return [[]]
  where projectionsFromVariable :: InterAlpha -> CReader [[TauDown]]
        projectionsFromVariable a = do
          -- TODO: care about history; the map fst destroys it
          ctaus <- map fst <$> Set.toList <$> concretizeType a
          -- cprojss: all projections for each concrete type
          concat <$> mapM (flip tProj proj) ctaus

{-
-- TODO: remove - broken!
tProj :: TauDown -> ProjTerm -> CReader [TauDown]
tProj tau proj =
  case (tau, proj) of
    (TdPrim p, ProjPrim p') | p == p' -> return [tau]
    (TdLabel lbl _, ProjLabel lbl') | lbl == lbl' -> return [tau]
    (TdOnion a1 a2, _) -> do -- CReader
      t1projs <- typesFromTypeVariable a1
      t2projs <- typesFromTypeVariable a2
      return $ t2projs ++ t1projs
    (TdOnionSub a s, _) ->
      if tSubProj s proj
        then return []
        else typesFromTypeVariable a
    (TdOnionProj a s, _) ->
      if not $ tSubProj s proj
         then return []
         else typesFromTypeVariable a
    (TdScape _, ProjFunc) -> return [tau]
    _ -> return []
  where typesFromTypeVariable :: InterAlpha -> CReader [TauDown]
        typesFromTypeVariable a = do
          ths <- concretizeType a
          -- TODO: care about history
          let ts = Set.toList $ Set.map fst ths
          concat <$> mapM (flip tProj proj) ts

-- TODO: remove!
-- Performs a check to ensure that projection can occur through an onion
-- subtraction type.
tSubProj :: ProjTerm -> ProjTerm -> Bool
tSubProj s proj =
  case (proj,s) of
    (ProjPrim p, ProjPrim p') -> p == p'
    (ProjLabel n, ProjLabel n') -> n == n'
    (ProjFunc, ProjFunc) -> True
    _ -> False
-}

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
substituteVars :: Constraints
               -> ForallVars
               -> InterAlpha
               -> Constraints
substituteVars constraints forallVars callSiteAlpha =
  (\(a, _, b) -> Set.union a b) $ runRWS
    (substituteAlpha constraints)
    (forallVars, callSiteAlpha)
    ()

-- |Performs cycle detection on a set of constraints.
--cycleDetectGeneric :: forall a. (AlphaType a)
--                   => (Constraint -> Maybe (SomeAlpha a, SomeAlpha a))
--                        -- ^A function used to extract appropriate subtyping
--                        --  information from a constraint.  If the constraint
--                        --  represents a subtype of the appropriate variables,
--                        --  it returns Just that pair of variables with the
--                        --  subtype first.  Otherwise, it returns Nothing.
--                   -> Constraints
--                        -- ^The set of constraints to check for cycles.
--                   -> Set (Set (SomeAlpha a))
--                        -- ^A set of resulting cycles; each cycle is a set of
--                        --  type variables which can be equivocated.
--cycleDetectGeneric f cs =
--  -- Start by finding all appropriate constraints and creating the
--  -- cycle detecting triple from them.  The triple elements are a set of
--  -- visited type variables, the lower bounding type variable, and the
--  -- upper bounding type variable.  The set always contains the current
--  -- lower bounding type variable.
--  let initial = Set.fromList $ map (tupleToTriple Set.empty) $ map swap pairs in
--  let result = leastFixedPoint step $ initial in
--  Set.map (\(s,_,_) -> s) $ Set.filter (\(_,a,b) -> a == b) result
--  where step :: (AlphaType a)
--             => Set (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
--             -> Set (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
--        step s = Set.unions $ map onestep $ Set.toList s
--        onestep :: (AlphaType a)
--                => (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
--                -> Set (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
--        onestep x@(s,a,b) =
--          if a == b
--            then Set.singleton x
--            else
--              Set.insert x $ Set.fromList $ map (tupleToTriple s) $
--                do -- List
--                  (a',b') <- pairs
--                  guard $ a == a'
--                  return (b',b)
--        tupleToTriple :: (AlphaType a)
--                      => Set (SomeAlpha a)
--                      -> (SomeAlpha a, SomeAlpha a)
--                      -> (Set (SomeAlpha a), SomeAlpha a, SomeAlpha a)
--        tupleToTriple s (a,b) = (Set.insert a s, a, b)
--        pairs :: [(SomeAlpha a, SomeAlpha a)]
--        pairs = mapMaybe f $ Set.toList cs

--cycleDetectInter :: Constraints -> Set (Set InterAlpha)
--cycleDetectInter = cycleDetectGeneric $ \c ->
--  case c of
--    AlphaSubtype a b _ -> Just (a,b)
--    _ -> Nothing

--cycleDetectCell :: Constraints -> Set (Set CellAlpha)
--cycleDetectCell = cycleDetectGeneric $ \c ->
--  case c of
--    CellAlphaSubtype a b _ -> Just (a,b)
--    _ -> Nothing

ct :: (LowerBounded a) => Constraints -> a -> [(LowerBound a, Chain a)]
ct cs a = Set.toList $ runReader (concretizeType a) cs

-- closeCases :: Constraints -> Constraints
-- closeCases cs = Set.unions $ do
--   (Case a guards _) <- Set.toList cs
--   (tau, _) <- ct cs a
--   -- Given a concretization, the first guard which matches is the one that
--   -- applies.  We'll build a list of the matches and filter on it.
--   return $ maybe Set.empty Set.unions $ caseGuardResults cs guards tau
--
-- findCaseContradictions :: Constraints -> Constraints
-- findCaseContradictions cs = Set.fromList $ do
--   c@(Case a guards _) <- Set.toList cs
--   (tau, chain) <- ct cs a
--   guard $ isNothing $ caseGuardResults cs guards tau
--   return $ Bottom $ ContradictionCase c chain
--
-- -- |A utility function used in case analysis.  Given a reader constraint set,
-- --  a list of guards, and a type, this function produces Just a list of
-- --  constraints (if the type matched one of the guards) or Nothing if none of
-- --  the guards matched.
-- caseGuardResults :: Constraints -> [Guard] -> TauDown -> Maybe [Constraints]
-- caseGuardResults cs guards tau =
--   listToMaybe $ filter (not . null) $ map check guards
--   where check :: Guard -> [Constraints]
--         check (Guard tauChi cs') =
--           map (Set.union cs') $
--             runReader (patternCompatible tau tauChi) cs
--
closeApplications :: Constraints -> Constraints
closeApplications cs = Set.unions $ do -- List
  UpperSubtype a0' a1' a2' _ <- Set.toList cs
  (t1, _) <- ct cs a0'
  (a3', _) <- ct cs a1'
  (t2, _) <- ct cs a3'
  projection <- runReader (tProj t1 ProjFunc) cs
  -- (ScapeData foralls tpat ai ci)
  let li = do
             TdScape scapeData <- projection
             let ScapeData _ tpat _ _ = scapeData
                 compatList = runReader (patternCompatible t2 tpat) cs
             return (compatList, scapeData)
  (c', ScapeData foralls _ ai ci) <-
    map (first $ Set.unions . catMaybes) $
    (\(x, y) -> x ++ take 1 y) $
    span (any isNothing . fst) li
  return $
    substituteVars
      (Set.insert (ai <: a2' .: histFIXME) (Set.union c' ci))
      foralls a2'
--  TdScape (ScapeData foralls ai ao cs') <- runReader (tProj td ProjFunc) cs
--  let funcChain' = IAHead ai' ao' c funcChain
--      hist = ClosureApplication funcChain' caChain
--      cs'' = Set.union cs' $
--               Set.fromList [ Cell ca3 <: ai .: hist
--                            , ao <: ao' .: hist ]
--  return $ substituteVars cs'' foralls ai ai'

-- traceValues a0 a1 a2 a3 t1 t2 tpats = let ?conf = True in
--   trace (render $ vcat [makeDoc a0, makeDoc a1, makeDoc a2, makeDoc a3, makeDoc t1, makeDoc t2, makeDoc tpats]) tpats

findNonFunctionApplications :: Constraints -> Constraints
findNonFunctionApplications cs = Set.fromList $ do -- List
  UpperSubtype a0' a1' _ _ <- Set.toList cs
  (t1, _) <- ct cs a0'
  (a3', _) <- ct cs a1'
  (t2, _) <- ct cs a3'
  let tpats =
        [tpat |
         TdScape (ScapeData _ tpat _ _) <- runReader (tProj t1 ProjFunc) cs]
      compatibiltyList tpat = runReader (patternCompatible t2 tpat) cs
  guard $ all (any isNothing . compatibiltyList) tpats
--    (traceValues a0' a1' a2' a3' t1 t2 $ trace (let ?conf = True in display $ compatibiltyList $ head tpats) tpats)
  return $ Bottom histFIXME

closeLops :: Constraints -> Constraints
closeLops cs = Set.fromList $ do
  c@(LazyOpSubtype _ a1 a2 a _) <- Set.toList cs
  (td1, chain1) <- ct cs a1
  (td2, chain2) <- ct cs a2
  -- NOTE: assumes that all lops are int -> int -> int
  guard $ all (not . null) $ runReader (tProj td1 $ ProjPrim PrimInt) cs
  guard $ all (not . null) $ runReader (tProj td2 $ ProjPrim PrimInt) cs
  return $ TdPrim PrimInt <: a .: ClosureLop c chain1 chain2

findLopContradictions :: Constraints -> Constraints
findLopContradictions cs = Set.fromList $ do
  c@(LazyOpSubtype _ a1 a2 _ _) <- Set.toList cs
  -- NOTE: assumes that all lops are int -> int -> int
  (td, chain) <- ct cs a1 ++ ct cs a2
  if any null $ runReader (tProj td $ ProjPrim PrimInt) cs
    then return $ Bottom $ ContradictionLop c chain
    else mzero

propagateCellsForward :: Constraints -> Constraints
propagateCellsForward cs = Set.fromList $ do
  c@(CellGetSubtype a a1 _) <- Set.toList cs
  (a2, a2Chain) <- ct cs a
  (t2, t2Chain) <- ct cs a2
  let a2Chain' = CAHeadG (CellGet a1) c a2Chain
  return $ t2 <: a1 .: ClosureCellForward a2Chain' t2Chain

propagateCellsBackward :: Constraints -> Constraints
propagateCellsBackward cs = Set.fromList $ do
  c@(CellSetSubtype a a1 _) <- Set.toList cs
  (a2, a2Chain) <- ct cs a
  (t2, t2Chain) <- ct cs a1
  let a2Chain' = CAHeadS (CellSet a1) c a2Chain
  return $ t2 <: a2 .: ClosureCellBackward a2Chain' t2Chain

propagateImmutable :: Constraints -> Constraints
propagateImmutable cs = Set.fromList $ do
  (Immutable a1 _) <- Set.toList cs
  (a, _) <- ct cs a1
  case a of
    TdOnion a2 a3 -> [Immutable a2 histFIXME, Immutable a3 histFIXME]
    TdLabel _ a2 -> do
      (a3, _) <- ct cs a2
      [Immutable a3 histFIXME]
    _ -> []

findIllegalFinalAssignments :: Constraints -> Constraints
findIllegalFinalAssignments cs = Set.fromList $ do
  CellSetSubtype a _ _ <- Set.toList cs
  (a2, _) <- ct cs a
  Final a2' _ <- Set.toList cs
  guard $ a2 == a2'
  return $ Bottom histFIXME

findIllegalImmutableAssignments :: Constraints -> Constraints
findIllegalImmutableAssignments cs = Set.fromList $ do
  CellSetSubtype a _ _ <- Set.toList cs
  a2 <- ct cs a
  Immutable a2' _ <- Set.toList cs
  guard $ fst a2 == a2'
  return $ Bottom histFIXME

-- |This closure calculation function produces appropriate bottom values for
--  immediate contradictions (such as tprim <: tprim' where tprim != tprim').
closeSingleContradictions :: Constraints -> Constraints
closeSingleContradictions cs =
  Set.unions $ map ($ cs)
        [ id
        , findNonFunctionApplications
        , findLopContradictions
        , findIllegalFinalAssignments
        , findIllegalImmutableAssignments
        ]

closeAll :: Constraints -> Constraints
closeAll cs =
  Set.unions $ map ($ cs)
    [ id
    , closeApplications
    , closeLops
    , propagateCellsForward
    , propagateCellsBackward
    , propagateImmutable
    ]

-- |Calculates the transitive closure of a set of type constraints.
calculateClosure :: (?conf :: Cfg.Config) => Constraints -> Constraints
calculateClosure c = ddisp $ closeSingleContradictions $ leastFixedPoint closeAll $ ddisp $ c
  where ddisp x =
          if Cfg.debugging
            then trace ("{-----\n" ++ display x ++ "\n-----}") x
            else x

saHelper :: (AlphaSubstitutable a)
         => (a -> b) -> a -> TSubstM b
saHelper constr a = constr <$> substituteAlpha a

saHelper2 :: (AlphaSubstitutable a1, AlphaSubstitutable a2)
          => (a1 -> a2 -> b) -> a1 -> a2 -> TSubstM b
saHelper2 constr a1 a2 = constr <$> substituteAlpha a1 <*> substituteAlpha a2

instance AlphaSubstitutable TauDown where
  substituteAlpha td = case td of
    TdLabel n a -> saHelper (TdLabel n) a
    TdOnion a1 a2 -> saHelper2 TdOnion a1 a2
    TdScape pfd -> saHelper TdScape pfd
    TdOnionSub a s -> saHelper (`TdOnionSub` s) a
    TdOnionProj a s -> saHelper (`TdOnionProj` s) a
    TdPrim _ -> return td
    TdEmptyOnion -> return td

instance AlphaSubstitutable ScapeData where
  substituteAlpha
    (ScapeData (ForallVars alphas) alphaIn alphaOut constraints) =
      ScapeData (ForallVars alphas)
        <$> pure alphaIn
        <*> pure alphaOut
        <*> substituteAlpha' constraints
      -- The variables described by the forall list should never be replaced
      where substituteAlpha' :: (AlphaSubstitutable a)
                             => a -> TSubstM a
            substituteAlpha' =
              local newEnv . substituteAlpha
            newEnv (ForallVars forallVars, callSiteAlpha) =
              (ForallVars $ Set.difference forallVars alphas, callSiteAlpha)

csaHelper :: (AlphaSubstitutable a)
          => (a -> hist -> b)
          -> a -> hist -> TSubstM b
csaHelper constr a hist =
  constr <$> substituteAlpha a <*> pure hist

csaHelper2 :: (AlphaSubstitutable a1, AlphaSubstitutable a2)
           => (a1 -> a2 -> hist -> b)
           -> a1 -> a2 -> hist -> TSubstM b
csaHelper2 constr a1 a2 hist =
  constr <$> substituteAlpha a1 <*> substituteAlpha a2 <*> pure hist

csaHelper3 :: (AlphaSubstitutable a1,
               AlphaSubstitutable a2,
               AlphaSubstitutable a3)
           => (a1 -> a2 -> a3 -> hist -> b)
           -> a1 -> a2 -> a3 ->  hist -> TSubstM b
csaHelper3 constr a1 a2 a3 hist =
  constr
    <$> substituteAlpha a1
    <*> substituteAlpha a2
    <*> substituteAlpha a3
    <*> pure hist

-- |A type alias for environments for alpha substitution.  This represents the
--  superscript argument list for the application substitution relation.
type AlphaSubstitutionEnv = (ForallVars, InterAlpha)
-- |A monad in which alpha substitution occurs.
type TSubstM a = RWS AlphaSubstitutionEnv Constraints () a

-- |A typeclass for entities which can substitute their type variables.
class AlphaSubstitutable a where
  -- |The alpha in the reader environment is added to superscripts.
  --  The set in the reader environment contains alphas to ignore.
  substituteAlpha :: a -> TSubstM a

-- Creates a new contour from an existing type variable
tContour :: InterAlpha -> Contour
tContour a =
  if elem aid $ unContour cntr
    then cntr -- TODO: consider changing TContour in the theory
              --       this should chop the list off at the dupe
    else contour $ aid:(unContour cntr)
  where cntr = alphaContour a
        aid = alphaId a

substituteAlphaHelper :: (Alpha a)
                      => a -> TSubstM a
substituteAlphaHelper a = do
  (ForallVars forallVars, cntrAlpha) <- ask
  let newContour = tContour cntrAlpha
  if not $ Set.member (alphaWeaken a) forallVars
    then return a
    -- The variable we are substituting should never have marked
    -- call sites.  The only places where polymorphic function
    -- constraints (forall constraints) are built are by the
    -- inference rules themselves (which have no notion of call
    -- sites) and the type replacement function (which does not
    -- replace forall-ed elements within a forall constraint).
    else assertEmptyContour a $
         return (setAlphaContour a newContour)
  where assertEmptyContour :: (Alpha a) => a -> b -> b
        assertEmptyContour alpha v =
          assert ((length . unContour . alphaContour) alpha == 0) v

instance (AlphaType a) => AlphaSubstitutable (SomeAlpha a) where
  substituteAlpha = substituteAlphaHelper

instance AlphaSubstitutable AnyAlpha where
  substituteAlpha = substituteAlphaHelper

instance AlphaSubstitutable Constraint where
  substituteAlpha c = case c of
      LowerSubtype td a hist ->
        csaHelper2 LowerSubtype td a hist
      UpperSubtype a ca ia hist ->
        csaHelper3 UpperSubtype a ca ia hist
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
      Final a hist ->
        csaHelper Final a hist
      Immutable a hist ->
        csaHelper Immutable a hist
      Bottom hist -> return $ Bottom hist

instance AlphaSubstitutable PatternType where
  substituteAlpha c =
    case c of
      Pattern a pp -> Pattern <$> substituteAlpha a <*> substituteAlpha pp

instance AlphaSubstitutable PrimaryPatternType where
  substituteAlpha c =
    case c of
      PatLabel lbl a pp ->
        PatLabel lbl <$> substituteAlpha a <*> substituteAlpha pp
      PatOnion pps -> PatOnion <$> mapM substituteAlpha pps
      _ -> return c
--      PatPrim prim -> return $ PatPrim prim
--      PatFun -> return $ PatFun

instance (Ord a, AlphaSubstitutable a) => AlphaSubstitutable (Set a) where
  substituteAlpha = fmap Set.fromList . mapM substituteAlpha . Set.toList