{-# LANGUAGE TupleSections #-}
module Language.LittleBang.Types.Closure
( calculateClosure
) where

import qualified Language.LittleBang.Types.Types as T
import Language.LittleBang.Types.Types ( (<:)
                                    , (.:)
                                    , Constraints
                                    , Constraint
                                    )
import Language.LittleBang.Types.UtilTypes (LabelName)

import Data.Maybe.Utils (justIf)
import Data.Function.Utils (leastFixedPoint)

import Control.Exception(assert)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromJust, mapMaybe, listToMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (mappend)

-- |A function which checks immediate compatability and produces an appropriate
--  constraint set for matches.  This function takes the type of a value, the
--  guard in a match case, and produces a constraint result.  If the result is
--  Nothing, the type is not compatible with the guard; otherwise, the set
--  provided should be added to the constraint set if this branch is chosen.
--  This function corresponds both to the relation <:: and to the mu function
--  in the notation.
createMatchConstraints :: T.ConstraintHistory
                       -> T.TauDownOpen
                       -> T.TauChi
                       -> Maybe Constraints
createMatchConstraints history tau chi =
    case (tau,chi) of
        (_,T.ChiTop) -> Just Set.empty
        (T.TdoPrim p, T.ChiPrim p') -> Set.empty `justIf` (p == p')
        (T.TdoLabel n t, T.ChiLabel n' a) ->
            let constraint = (T.toTauDownClosed t <: T.TucAlpha a .: history) in
            (Set.singleton constraint) `justIf` (n == n')
        (T.TdoFunc _, T.ChiFun) -> Just Set.empty
        (T.TdoOnion t t', _) ->
            let mc1 = createMatchConstraints history t chi in
            let mc2 = createMatchConstraints history t' chi in
            maybe mc2 Just mc1
        _ -> Nothing

findTauDownOpen :: Constraints -> Constraints
findTauDownOpen = Set.fromList . catMaybes . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype a _ _ -> fmap (const c) $ T.toTauDownOpen a
            _               -> Just c

findAlphaOnRight :: Constraints
                 -> Map T.Alpha (Set (T.TauDownClosed, Constraint))
findAlphaOnRight = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype a (T.TucAlpha b) _ -> Map.singleton b $
                                              Set.singleton (a, c)
            _                            -> Map.empty

findAlphaUpOnRight :: Constraints
                   -> Map T.AlphaUp (Set (T.TauDownClosed, Constraint))
findAlphaUpOnRight = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype a (T.TucAlphaUp b) _ -> Map.singleton b $
                                                Set.singleton (a, c)
            _                              -> Map.empty

findAlphaOnLeft :: Constraints
                -> Map T.Alpha (Set (T.TauUpClosed, Constraint))
findAlphaOnLeft = Map.unionsWith mappend . map fn . Set.toList
  where fn c = 
          case c of
            T.Subtype (T.TdcAlpha a) b _ -> Map.singleton a $
                                              Set.singleton (b, c)
            _                            -> Map.empty

findLblAlphaOnLeft :: Constraints
                   -> Map T.Alpha (Set ( LabelName
                                       , T.TauUpClosed
                                       , Constraint))
findLblAlphaOnLeft = Map.unionsWith mappend . map fn . Set.toList
  where fn c = 
          case c of
            T.Subtype (T.TdcLabel lbl (T.TdcAlpha a)) b _ ->
              Map.singleton a $ Set.singleton (lbl, b, c)
            _ -> Map.empty

findPolyFuncs :: Constraints
              -> Map T.AlphaUp (Set (T.Alpha, T.PolyFuncData, Constraint))
findPolyFuncs = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype (T.TdcFunc pfd) (T.TucFunc au a) _ ->
                Map.singleton au $ Set.singleton (a, pfd, c)
            _ -> Map.empty

findAlphaAmpPairs :: Constraints
                  -> Map (T.Alpha, T.Alpha) (Set ( T.TauUpClosed
                                                 , Constraint))
findAlphaAmpPairs = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Subtype (T.TdcOnion (T.TdcAlpha a) (T.TdcAlpha b)) d _ ->
              Map.singleton (a,b) $ Set.singleton (d, c)
            _ -> Map.empty

findCases :: Constraints -> Map T.AlphaUp (Set ([T.Guard], Constraint))
findCases = Map.unionsWith mappend . map fn . Set.toList
  where fn c =
          case c of
            T.Case au gs _ -> Map.singleton au $ Set.singleton (gs, c)
            _ -> Map.empty

-- |This function transforms a specified alpha up into a call site list.  The
--  resulting call site list is in the reverse order form dictated by the
--  CallSites structure; that is, the list [{'3},{'2},{'1}] represents the type
--  variable with the exponent expression '1^('2^'3).  The resulting call site
--  list is suitable for use in type variable substitution for polymorphic
--  functions.
addToCallSites :: T.AlphaUp -> T.CallSites
addToCallSites alphaUp =
    let alphaIdx = T.getIndex alphaUp
        alphaEntry = T.AlphaUp $ T.AlphaContents alphaIdx $ T.callSites []
        siteList = T.unCallSites $ T.getCallSites alphaUp
        siteList' = map (\(T.CallSite a) -> a) siteList
        totals = zip siteList' $ tail $ scanl Set.union Set.empty siteList'
        rest = dropWhile (\(_,b) -> not $ Set.member alphaEntry b) totals
    in
    T.callSites $
    case rest of
            [] -> -- In this case, this call site is new to the list
                (T.CallSite $ Set.singleton alphaEntry):siteList
            (_,cyc):tl -> -- In this case, we found a cycle
                (T.CallSite cyc):(map (T.CallSite . fst) tl)

-- |A function which performs substitution on a set of constraints.  All
--  variables in the alpha set are replaced with corresponding versions that
--  have the specified alpha up in their call sites list.
substituteVars :: Constraints -> Set T.AnyAlpha -> T.AlphaUp -> Constraints
substituteVars constraints forallVars replAlpha = substituteAlpha f constraints
  where newCallSites = addToCallSites replAlpha
        -- |Separates an AnyAlpha into an (AlphaContents -> AnyAlpha) and the
        --  parts of an AlphaContents
        separate sa = case sa of
            T.SomeAlpha (T.Alpha (T.AlphaContents i sites)) ->
                (T.SomeAlpha . T.Alpha, i, sites)
            T.SomeAlphaUp (T.AlphaUp (T.AlphaContents i sites)) ->
                (T.SomeAlphaUp . T.AlphaUp, i, sites)
        f sa =
            let (constr,i,sites) = separate sa in
            if not $ Set.member sa $ forallVars
                then sa
                -- The variable we are substituting should never have marked
                -- call sites.  The only places where polymorphic function
                -- constraints (forall constraints) are built are by the
                -- inference rules themselves (which have no notion of call
                -- sites) and the type replacement function (which does not
                -- replace forall-ed elements within a forall constraint).
                else assert ((length $ T.unCallSites sites) == 0) $
                     constr $ T.AlphaContents i $ newCallSites

-- |Calculates the constraints produced from a given constraint set by the
--  transitivity rules.  This rule performs transitivity for both alphas and
--  alpha-ups.
closeTransitivity :: Constraints -> Constraints
closeTransitivity cs = Set.fromList $
                  concat $ 
                      Map.elems $
                      Map.intersectionWith subtypeCrossProduct lefts rights
  where tdoCs  = findTauDownOpen cs
        lefts  = findAlphaOnRight tdoCs
        rights = findAlphaOnLeft cs
        subtypeCrossProduct xs ys =
          [ x <: y .: T.ClosureTransitivity xc yc |
            (x, xc) <- Set.toList xs, (y, yc) <- Set.toList ys ]

closeLabels :: Constraints -> Constraints
closeLabels cs = Set.fromList $
            concat $
            Map.elems $
            Map.intersectionWith fn lefts rights
  where tdoCs    = findTauDownOpen cs
        lefts    = findAlphaOnRight tdoCs
        rights   = findLblAlphaOnLeft cs
        fn xs ys =
          [ T.TdcLabel lbl x <: y .: T.ClosureLabel xc yc |
            (x, xc) <- Set.toList xs, (lbl, y, yc) <- Set.toList ys ]

closeOnions :: Constraints -> Constraints
closeOnions cs = Set.fromList $ allTrans lefts $ Map.toList rights
  where tdoCs  = findTauDownOpen cs
        lefts  = findAlphaOnRight tdoCs
        rights = findAlphaAmpPairs cs
        tryTrans alphas ((a1, a2), tucs) = do
          t1 <- Map.lookup a1 alphas
          t2 <- Map.lookup a2 alphas
          return
            [ T.TdcOnion t1' t2' <: tuc .: T.ClosureOnion c1 c2 c3 |
              (t1', c1) <- Set.toList t1,
              (t2', c2) <- Set.toList t2,
              (tuc, c3) <- Set.toList tucs ]
        allTrans alphas amps = concat $ catMaybes $ map (tryTrans alphas) amps

closeCases :: Constraints -> Constraints
closeCases cs = Set.unions $ map pickGuardConstraints tausToGuards
  where lefts = findAlphaUpOnRight $ findTauDownOpen cs
        cases = findCases cs
        tausToGuards = filterOpens $ concat $ Map.elems $
                Map.intersectionWith
                  (\lEnts cEnts -> [(l,c)
                    | l <- Set.toList lEnts, c <- Set.toList cEnts])
                  lefts
                  cases
        filterOpens xs = mapMaybe f xs
          where f ((tdc, a), (b, c)) =
                  T.toTauDownOpen tdc >>= return . (\x -> ((x, a), (b, c)))
        pickGuardConstraints ((tauDownOpen, tdoc), (guards, gc)) =
            let resultConstraints = catMaybes
                    [ fmap (Set.union constr) $
                          createMatchConstraints (T.ClosureCase tdoc gc)
                                                 tauDownOpen
                                                 pat
                    | T.Guard pat constr <- guards]
            in
            maybe (Set.singleton $ T.Bottom $ T.ContradictionCase tdoc gc) id $
                  listToMaybe resultConstraints

closeApplications :: Constraints -> Constraints
closeApplications cs =
    Set.unions $ map pickPolyConstraints $ concatMap expandIntoCases $
            Map.toList premiseDataByAlphaUp
  where concretes = findAlphaUpOnRight $ findTauDownOpen cs
        polyfuncs = findPolyFuncs cs
        premiseDataByAlphaUp = Map.intersectionWith (,) concretes polyfuncs
        expandIntoCases (alphaUp, (cs', pfs)) =
                [ (alphaUp, el, y) | el <- Set.toList cs', y <- Set.toList pfs]
        pickPolyConstraints ( alphaIn
                            , (tauDownOpen, tdoc)
                              , ( alphaOut
                                , T.PolyFuncData
                                    forallVars
                                    polyAlphaIn
                                    polyAlphaOut
                                    polyC 
                                , pfc
                                )
                            ) =
            let polyC' = Set.union polyC $
                         Set.fromList $
                         map ( .: T.ClosureApplication tdoc pfc)
                    [ T.toTauDownClosed tauDownOpen <: T.TucAlpha polyAlphaIn,
                      T.TdcAlpha polyAlphaOut <: T.TucAlpha alphaOut]
            in
            substituteVars polyC' forallVars alphaIn

-- |This closure calculation function produces appropriate bottom values for
--  immediate contradictions (such as tprim <: tprim' where tprim != tprim').
closeSingleContradictions :: Constraints -> Constraints
closeSingleContradictions cs = Set.fromList $ catMaybes $
    [
        case x of
            T.Subtype a b _ -> checkSubtype x (a,b)
            _ -> Nothing
    |
        x <- Set.toList cs
    ]
  where
    checkSubtype constraint (a,b) = case (a,b) of
        (T.TdcPrim p, T.TucPrim p')        ->
          T.Bottom (T.ContradictionPrimMismatch constraint) `justIf` (p /= p')

        (T.TdcLabel _ _, T.TucPrim _)      ->
          Just $ T.Bottom $ T.ContradictionLabelPrim constraint

        (T.TdcPrim _, T.TucFunc _ _)       ->
          Just $ T.Bottom $ T.ContradictionPrimFunc constraint

        (T.TdcFunc _, T.TucPrim _)         ->
          Just $ T.Bottom $ T.ContradictionFuncPrim constraint

        (T.TdcLabel _ _, T.TucFunc _ _)    ->
          Just $ T.Bottom $ T.ContradictionLabelFunc constraint

        _                                  ->
          Nothing

closeAll :: Constraints -> Constraints
closeAll c = Set.unions $ map ($ c)
        [ id
        , closeTransitivity
        , closeLabels
        , closeOnions
        , closeCases
        , closeApplications
        , closeSingleContradictions ]

-- |Calculates the transitive closure of a set of type constraints.
calculateClosure :: Constraints -> Constraints
calculateClosure c = leastFixedPoint closeAll c


-- |A typeclass for entities which can substitute their type variables.
class AlphaSubstitutable a where
    substituteAlpha :: (T.AnyAlpha -> T.AnyAlpha) -> a -> a

instance AlphaSubstitutable T.AlphaUp where
    substituteAlpha f au =
        case substituteAlpha f $ T.SomeAlphaUp au of
            T.SomeAlphaUp au' -> au'
            _ -> error "substituteAlpha function argument produced bad output"

instance AlphaSubstitutable T.Alpha where
    substituteAlpha f a =
        case substituteAlpha f $ T.SomeAlpha a of
            T.SomeAlpha a' -> a'
            _ -> error "substituteAlpha function argument produced bad output"

instance AlphaSubstitutable T.AnyAlpha where
    substituteAlpha f = f

instance AlphaSubstitutable T.TauUpOpen where
    substituteAlpha f x =
        -- The toTauUpOpen will never give Nothing here.  We have that
        -- T.toTauUpOpen . T.toTauUpClosed is equivalent to Just and the
        -- substituteAlpha routine will not insert any alphas where there
        -- were not alphas before.
        fromJust $ T.toTauUpOpen $ substituteAlpha f $ T.toTauUpClosed x

instance AlphaSubstitutable T.TauUpClosed where
    substituteAlpha f x = case x of
        T.TucPrim p -> T.TucPrim $ substituteAlpha f p
        T.TucFunc au a ->
                T.TucFunc (substituteAlpha f au) $ substituteAlpha f a
        T.TucTop -> T.TucTop
        T.TucAlphaUp a -> T.TucAlphaUp $ substituteAlpha f a
        T.TucAlpha a -> T.TucAlpha $ substituteAlpha f a

instance AlphaSubstitutable T.TauDownOpen where
    substituteAlpha f x =
        -- fromJust is safe here for the same reasons as in TauUpOpen
        fromJust $ T.toTauDownOpen $ substituteAlpha f $ T.toTauDownClosed x

instance AlphaSubstitutable T.TauDownClosed where
    substituteAlpha f x = case x of
        T.TdcPrim p -> T.TdcPrim $ substituteAlpha f p
        T.TdcLabel n t -> T.TdcLabel n $ substituteAlpha f t
        T.TdcOnion t1 t2 -> T.TdcOnion (substituteAlpha f t1) $
                substituteAlpha f t2
        T.TdcFunc pfd -> T.TdcFunc $ substituteAlpha f pfd
        T.TdcTop -> T.TdcTop
        T.TdcAlpha a -> T.TdcAlpha $ substituteAlpha f a

instance AlphaSubstitutable T.PolyFuncData where
    substituteAlpha f (T.PolyFuncData alphas alphaIn alphaOut constraints) =
        -- The variables described by the forall list should never be replaced
        T.PolyFuncData alphas (substituteAlpha f' alphaIn)
                (substituteAlpha f' alphaOut) (substituteAlpha f' constraints)
      where f' aa =
                if aa `Set.member` alphas
                    then aa
                    else f aa

instance AlphaSubstitutable T.PrimitiveType where
    substituteAlpha _ p = p

instance AlphaSubstitutable T.Constraint where
    substituteAlpha f c = case c of
        T.Subtype tdc tuc hist ->
          T.Subtype
            (substituteAlpha f tdc)
            (substituteAlpha f tuc)
            hist
        T.Case au guards hist ->
          T.Case
            (substituteAlpha f au)
            (substituteAlpha f guards)
            hist
        T.Bottom hist -> T.Bottom hist

instance AlphaSubstitutable T.Guard where
    substituteAlpha f (T.Guard tauChi constraints) =
        T.Guard (substituteAlpha f tauChi) $ substituteAlpha f constraints

instance AlphaSubstitutable T.TauChi where
    substituteAlpha f c = case c of
        T.ChiPrim p -> T.ChiPrim $ substituteAlpha f p
        T.ChiLabel n au -> T.ChiLabel n $ substituteAlpha f au
        T.ChiFun -> T.ChiFun
        T.ChiTop -> T.ChiTop

instance (AlphaSubstitutable a) => AlphaSubstitutable [a] where
    substituteAlpha = map . substituteAlpha

instance (Ord a, AlphaSubstitutable a) => AlphaSubstitutable (Set a) where
    substituteAlpha = Set.map . substituteAlpha



