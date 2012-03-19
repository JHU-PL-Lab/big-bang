module Language.MicroBang.Interpreter.Interpreter
  ( evalTop
  , EvalError(..)
  , EvalSuccessOrFailure(..)
  , EvalStringResult(..)
  , EvalStringM
  , EvalStringResultErrorWrapper(..)) where

import Control.Monad (guard)
import Control.Monad.RWS (RWS, evalRWS, censor)
import Control.Monad.Error (Error, ErrorT, strMsg, throwError, runErrorT)
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask, local)
import Control.Monad.Writer (tell, listen)

import Data.Monoid (Monoid, mempty)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, fold)
import qualified Data.Set as Set


import qualified Language.MicroBang.Syntax.Parser as P


import Language.MicroBang.Ast as AST
  --( Expr(..)
  --, Operator(..)
  --, Chi(..)
  --, Branches
  --, Branch(..)
  --, Value(..)
  --, exprFromValue
  --, Evaluated(..)
  --)
import Language.MicroBang.Types.UtilTypes (LabelName, Ident, unIdent)
import qualified Language.MicroBang.Types.UtilTypes as UT
import Utils.Render.Display


data ConstraintEvaluatorError
      -- |Indicates that an expression contained an unbound variable.
      = NotClosed Ident
      -- |Indicates that the binder and label use the same variable.
      | DoubleBound Ident
      -- TODO: Add NotImplemented
    deriving (Show, Eq)
instance Error ConstraintEvaluatorError where
    strMsg = error
instance Display ConstraintEvaluatorError where
    makeDoc err = case err of
        NotClosed i ->
          text "TypeInference: not closed:" <+>
          (text $ unIdent i)
        DoubleBound i ->
          text "TypeInference: bound both in the binder and in the label:" <+>
          (text $ unIdent i)

-- Program Points
type ProgramPointLabel = Integer
data ProgramPoint = ProgramPoint ProgramPointLabel [Set ProgramPointLabel]
  deriving (Show, Eq, Ord)

data PI = PInt | PUnit | PLabel LabelName | PFun
  deriving (Show, Eq, Ord)
data SX = XAny | XTInt | XTUnit | XLabel LabelName ProgramPoint | XFun
  deriving (Show, Eq, Ord)
type SUp = (ProgramPoint, ProgramPoint)

data SDown
  = SDUnit
  | SDInt Integer
  | SDLabel LabelName ProgramPoint
  | SDOnion ProgramPoint ProgramPoint
  | SDOnionSub ProgramPoint FOnionSub
  | SDEmptyOnion
  | SDFunction (Set ProgramPoint) ProgramPoint ProgramPoint Constraints
  | SDBadness
  deriving (Show, Eq, Ord)
data FOnionSub = FSubInt | FSubUnit | FSubLabel LabelName | FSubFun
  deriving (Show, Eq, Ord)

convertSubTermToFOnionSub :: UT.SubTerm -> FOnionSub
convertSubTermToFOnionSub (UT.SubPrim (UT.PrimInt)) = FSubInt
convertSubTermToFOnionSub (UT.SubPrim (UT.PrimUnit)) = FSubUnit
convertSubTermToFOnionSub (UT.SubLabel ln) = FSubLabel ln
convertSubTermToFOnionSub UT.SubFunc = FSubFun

type Fdot = Maybe Constraints
-- Constraints
-- |A type alias defining the form of constraints.
type Constraints = Set Constraint
-- |A type describing constraints in Little Bang.
data Constraint
  = SLower SDown ProgramPoint
  | SIntermediate ProgramPoint ProgramPoint
  | SUpper ProgramPoint SUp
  | SOp ProgramPoint Operator ProgramPoint ProgramPoint
  | SCase ProgramPoint [(SX, Constraints)]
  deriving (Show, Ord, Eq)


-- Errors

-- I.EvalError, From TinyBang Interpreter.
data EvalError =
      NoConcreteValue
    | NonFunctionApplication
    | CaseContradiction
    | IntegerOperationFailure
    | ConstraintFailure ConstraintEvaluatorError
    deriving (Eq, Show)
instance Error EvalError where
  strMsg = error
instance Display EvalError where
  makeDoc ee =
    case ee of
      NoConcreteValue -> text "No Concrete Value"
      NonFunctionApplication -> text "NonFunction Application"
      CaseContradiction -> text "Case Contradiction"
      IntegerOperationFailure -> text "Integer Operation Failure"
      ConstraintFailure cee -> text $ "Constraint Failure" ++ show cee
-- use makeDoc to call recursively to print subtypes
-- use <+> to concatenate with recursive makeDoc calls


type EvalStringM a = Either EvalStringResultErrorWrapper a

-- From TinyBang SourceInterpreter.
data EvalSuccessOrFailure = EvalSuccess Value | EvalFailure EvalError
instance Display EvalSuccessOrFailure where
  makeDoc esof =
    case esof of
      EvalSuccess v -> text "EvalS:" <+> (makeDoc v)
      EvalFailure ee-> text "EvalF" <+> (makeDoc ee)


-- |A result type for evalStringTop
data EvalStringResult
    = EvalResult Expr EvalSuccessOrFailure
    | Contradiction Expr Constraints
        -- ^Represents a contradiction appearing in a constraint set.  The
        --  indicated set should contain at least one contradiction.
    | DerivationFailure Expr Constraints --TODO: add middle thingy
    | ParseFailure P.ParseError
    | LexFailure String
instance Display EvalStringResult where
  makeDoc esr =
    case esr of
      EvalResult e esof -> text "Result:" <+> (makeDoc e) <+> (makeDoc esof)
      Contradiction e cs-> text $ "Contradiction" ++ show e ++ " " ++ show cs
      DerivationFailure e cs -> text $ "DerivationFailure" ++ show e ++ " " ++ show cs
      ParseFailure pe -> text $ "ParseFailure" ++ show pe
      LexFailure s -> text "LexFailure" <+> text s


-- |A monadic exception type for the evalStringTop process.
data EvalStringResultErrorWrapper =
    FailureWrapper EvalStringResult
instance Error EvalStringResultErrorWrapper where
    strMsg = error

freshVar :: CEM ProgramPoint
freshVar = do
    idx <- get
    put $ idx + 1
    return $ ProgramPoint idx []



type PId = Integer
type M = Map Ident ProgramPoint
type NextFreshVar = PId

type CEM a = ErrorT ConstraintEvaluatorError
                    (RWS M Constraints NextFreshVar)
                    a

-- local: local (Map.insert key value) (recursive call)
-- ask: returns a M
-- tell: write (Set Constraint)
-- get/put:

-- Start here. :)
evalTop :: Expr -> Either EvalError Value
evalTop e =
  let cemProgPt = derive e in
  let (progPt, cs) = runCEM cemProgPt (Map.empty) 0 in
    case progPt of
      Left f -> Left (ConstraintFailure f)
      Right p ->
        let cs' = close cs in
          Right (retrieve p cs')

capture :: (M -> M) -> Expr -> CEM (ProgramPoint, Constraints)
capture f e = censor (const mempty) $ listen $ local f $ derive e

derive :: Expr -> CEM ProgramPoint
derive (Var i) = do -- M|-x:p1\Null, where x:p1 E M
  m <- ask
  let p1 = Map.lookup i m
  maybe (throwError $ NotClosed i) (return) p1
  -- the maybe just does this:
  --case p1 of
  --  Nothing -> throwError $ NotClosed i
  --  Just p -> return p
derive (Label ln e) = do
  -- p1 \ {lbl p2<:p1}U F, where e:p2\F
  p2 <- (derive e)
  p1 <- freshVar
  tell (Set.singleton (SLower (SDLabel ln p2) p1))
  return p1
derive (BinOp op e1 e2) = do
  p1 <- derive e1
  p2 <- derive e2
  p0 <- freshVar
  tell (Set.singleton (SOp p1 op p2 p0))
  return p0
derive (Appl e1 e2) = do
  p1 <- derive e1
  p2 <- derive e2
  p1' <- freshVar
  p2' <- freshVar
  tell (Set.singleton (SUpper p1 (p1', p2')))
  tell (Set.singleton (SIntermediate p2 p1'))
  return p2'
derive (Case e bs) = do
  p2' <- derive e
  p3' <- freshVar
  p1' <- freshVar
  let (ms, sxs) = unzip $ Prelude.map (edigestbranch p2' p3') bs
  trips <- sequence $ zipWith3 (capturebranch) ms sxs bs
  tell (Set.singleton (SCase p2' (Prelude.map (convertToConstraint p1') trips)))
  return p1'
  where
    convertToConstraint :: ProgramPoint -> (SX, ProgramPoint, Constraints) -> (SX, Constraints)
    convertToConstraint p1 (sx, pn, cs) =
      (sx, Set.union cs (Set.singleton (SIntermediate pn p1)))

    capturebranch :: M -> SX -> Branch -> CEM (SX, ProgramPoint, Constraints)
    capturebranch m sx (Branch _ _ eb) = do
      (p, cs) <- capture (Map.union m) eb
      return (sx, p, cs)

    edigestbranch :: ProgramPoint -> ProgramPoint -> Branch -> (M, SX)
    edigestbranch p1 p2 (Branch mi x _) =
      let m' = (case mi of
              Nothing -> Map.empty
              Just ix -> Map.insert ix p1 Map.empty)
        in
      (case x of
              ChiAny -> (m', XAny)
              ChiPrim UT.PrimInt -> (m', XTInt)
              ChiPrim UT.PrimUnit -> (m', XTUnit)
              ChiLabel ln li -> case mi of
                Nothing -> ((Map.insert li p2 m'), XLabel ln p2)
                Just ix -> if ix == li then error "bad branch"
                  else ((Map.insert li p2 m'), XLabel ln p2)
              ChiFun -> (m', XFun))

derive AST.PrimUnit = do
  p1 <- freshVar
  tell (Set.singleton (SLower SDUnit p1))
  return p1
derive (AST.PrimInt i) = do
  p1 <- freshVar
  tell (Set.singleton (SLower (SDInt i) p1))
  return p1
derive (Func ident e) = do
  p2 <- freshVar
  (p3, f) <- capture (Map.insert ident p2) e
  p1 <- freshVar
  let ps = p2 `Set.insert` (p3 `Set.insert` (extractPs f))
  tell (Set.singleton (SLower (SDFunction ps p2 p3 f) p1))
  return p1
derive EmptyOnion = do
  p1 <- freshVar
  tell (Set.singleton (SLower SDEmptyOnion p1))
  return p1
derive (Onion e1 e2) = do
  p1 <- derive e1
  p2 <- derive e2
  p0 <- freshVar
  tell (Set.singleton (SLower (SDOnion p1 p2) p0))
  return p0
derive (OnionSub e sub) = do
  p2 <- derive e
  p1 <- freshVar
  tell (Set.singleton (SLower (SDOnionSub p2 (convertSubTermToFOnionSub sub)) p1))
  return p1

extractPs :: Constraints -> Set ProgramPoint
extractPs cs = fold grabCs Set.empty cs where
  grabCs :: Constraint -> Set ProgramPoint -> Set ProgramPoint
  grabCs c ps = case c of
    (SLower sd p1) -> case sd of
      SDLabel _ p2 -> p1 `Set.insert` (p2 `Set.insert` ps)
      SDOnion p2 p3 -> p1 `Set.insert`
          (p2 `Set.insert` (p3 `Set.insert` ps))
      SDOnionSub p2 _ -> p1 `Set.insert` (p2 `Set.insert` ps)
      SDFunction pl p2 p3 f -> (p1 `Set.insert`
                (p2 `Set.insert` (p3 `Set.insert` ps)))
                `Set.union` ((extractPs f) Set.\\ pl)
      _ -> p1 `Set.insert` ps
    (SIntermediate p1 p2) -> p1 `Set.insert` (p2 `Set.insert` ps)
    (SUpper p (p1,p2)) -> p `Set.insert` (p1 `Set.insert` (p2 `Set.insert` ps))
    (SOp p1 _ p2 p3) -> p1 `Set.insert` (p2 `Set.insert`
              (p3 `Set.insert` ps))
    (SCase p1 sxcs) -> p1 `Set.insert` (ps `Set.union` extractBranches sxcs)
      where
        extractBranches :: [(SX, Constraints)] -> Set ProgramPoint
        extractBranches sxcs' = foldr (Set.union . extractBranch) Set.empty sxcs'
        extractBranch :: (SX, Constraints) -> Set ProgramPoint
        extractBranch (sx, cs') =
          let ps' = case sx of
               (XLabel _ p) -> (Set.singleton p)
               _ -> Set.empty
          in
          ps' `Set.union` (extractPs cs')

runCEM :: CEM a -> M -> NextFreshVar -> (Either ConstraintEvaluatorError a, Constraints)
runCEM c r s = evalRWS (runErrorT c) r s

close :: Constraints -> Constraints
close cs =
  let newCs = Set.unions $
        map ($ cs) [id, app, caseAnalysis, addition, trueEq, falseEq] in
  error $ show newCs
  --run evaluation closure rules
  where
    app :: Constraints -> Constraints
    app cs0 = Set.unions $
      do
      (SUpper p0 (p1',p2')) <-Set.toList cs0
      s1 <- Set.toList $ concretization cs0 p0
      s2 <- Set.toList $ concretization cs0 p1'
      Just (SDFunction ps' p1 p2 f) <- Set.toList $ project cs0 s1 PFun
      return (Set.map (appSub (Set.toList ps') p1')(
          Set.insert (SLower s2 p1) $
          Set.insert (SIntermediate p2 p2') f))

    caseAnalysis cs0 = cs0

    addition cs0 = cs0

    trueEq cs0 = cs0

    falseEq cs0 = cs0

appSub :: [ProgramPoint] -> ProgramPoint -> Constraint -> Constraint
appSub ps0 p0 c = case c of
  SLower sd p1 -> SLower (subSDown ps0 p0 sd) (subProgPoint ps0 p0 p1)
  SIntermediate p1 p2 -> SIntermediate (subProgPoint ps0 p0 p1) (subProgPoint ps0 p0 p2)
  SUpper p1 (p2, p3) -> SUpper (subProgPoint ps0 p0 p1) ((subProgPoint ps0 p0 p2),(subProgPoint ps0 p0 p3))
  SOp p1 o p2 p3 -> SOp (subProgPoint ps0 p0 p1) o (subProgPoint ps0 p0 p2) (subProgPoint ps0 p0 p3)
  SCase p1 bs -> SCase (subProgPoint ps0 p0 p1) (map (subBranch ps0 p0) bs)
  where
    subProgPoint :: [ProgramPoint] -> ProgramPoint -> ProgramPoint -> ProgramPoint
    subProgPoint ps0' p0' p1@(ProgramPoint l pL) =
      if pL == [] then
        ( if any (==p1) ps0'
          then (ProgramPoint l (econtourlist p0'))
          else p1)
      else p1 --shouldn't be here...
      where
        econtourlist :: ProgramPoint -> [Set ProgramPointLabel]
        econtourlist (ProgramPoint l' pL') = (Set.singleton l') : pL'

    subSDown :: [ProgramPoint] -> ProgramPoint -> SDown -> SDown
    subSDown ps0' p0' sd = case sd of
      SDLabel ln p1 -> SDLabel ln (subProgPoint ps0' p0' p1)
      SDOnion p1 p2 -> SDOnion (subProgPoint ps0' p0' p1) (subProgPoint ps0' p0' p2)
      SDOnionSub p1 sub -> SDOnionSub (subProgPoint ps0' p0' p1) sub
      SDFunction ps1 p1 p2 cs ->
        let ps' = Set.toList ((Set.fromList ps0') Set.\\ ps1) in
        SDFunction ps1
          (subProgPoint ps' p0' p1)
          (subProgPoint ps' p0' p2)
          (Set.map (appSub ps' p0') cs)
      _ -> sd --unit, int, empty onion, badness

    subSX :: [ProgramPoint] -> ProgramPoint -> SX -> SX
    subSX ps0' p0' (XLabel ln p1) = (XLabel ln (subProgPoint ps0' p0' p1))
    subSX _ _ sx = sx

    subBranch :: [ProgramPoint] -> ProgramPoint -> (SX, Constraints) -> (SX, Constraints)
    subBranch ps0' p0' (sx, cs) = (subSX ps0' p0' sx, Set.map (appSub ps0' p0') cs)


flowCompatible :: Constraints -> SDown -> SX -> Set Fdot
flowCompatible cs0 sd sx =
  case (sd, sx) of
    (_, XAny) -> nil
    (SDUnit, XTUnit) -> nil
    (SDUnit, _) -> splat
    (SDInt _, XTInt) -> nil
    (SDInt _, _) -> splat
    (SDFunction _ _ _ _, XFun) -> nil
    (SDFunction _ _ _ _, _) -> splat
    (SDLabel sln sp, XLabel xln xp) | sln == xln ->
        one (SIntermediate sp xp)
    (SDLabel _ _, _) -> splat

    --any case already taken care of above
    (SDOnion p1 p2, _) ->
      fold (Set.union) Set.empty $ Set.map flowOnion (concretization cs0 p2)
      where
        flowOnion :: SDown -> Set Fdot
        flowOnion s = let fl2 = flowCompatible cs0 s sx in
          if fl2 == splat then (flowLots (concretization cs0 p1))
          else fl2
        flowLots :: Set SDown -> Set Fdot
        flowLots = fold flowCompatibleUnion Set.empty
        flowCompatibleUnion :: SDown -> Set Fdot -> Set Fdot
        flowCompatibleUnion s = Set.union (flowCompatible cs0 s sx)
    --any case already taken care of above
    (SDOnionSub p1 sub, _) ->
      if esubmatch sub sx then
        splat
      else
        fold (\s outset -> Set.union outset (flowCompatible cs0 s sx))
            Set.empty (concretization cs0 p1)
      where
        esubmatch :: FOnionSub -> SX -> Bool
        esubmatch FSubInt XTInt = True
        esubmatch FSubUnit XTUnit = True
        esubmatch (FSubLabel sln) (XLabel xln _) | sln == xln = True
        esubmatch FSubFun XFun = True
        esubmatch _ _ = False
    (SDEmptyOnion, _) -> splat
    (SDBadness, _) -> splat
  where
    nil = Set.singleton $ Just Set.empty
    splat = Set.singleton Nothing
    one c = Set.singleton (Just (Set.singleton c))

-- Find all the s' for which "SDown is projected by SX as s'"
project :: Constraints -> SDown -> PI -> Set (Maybe SDown)
project cs0 sd pi0 =
  case (sd, pi0) of
    (SDUnit, PUnit) -> Set.singleton (Just SDUnit)
    (SDUnit, _) -> Set.singleton Nothing

    (SDInt _, PInt) -> Set.singleton (Just sd)
    (SDInt _, _) -> Set.singleton Nothing

    (SDLabel ln _, PLabel pln) | ln == pln -> Set.singleton (Just sd)
    (SDLabel _ _, _) -> Set.singleton Nothing

    (SDOnion p1 p2, pi1) ->
      let s2s = concretization cs0 p2 in
      let proj2 = (projectLots s2s pi1) Set.\\ (Set.singleton Nothing) in
      proj2
      --Onion Right Projection (above)
      `Set.union`
      -- Onion Left Projection (below)
      (if (Set.toList proj2) == [] then
          (let s1s = concretization cs0 p1 in
            projectLots s1s pi1)
        else
          Set.empty)

    (SDOnionSub p1 fos1, pi1) ->
      if tsubproj fos1 pi1 then
        Set.singleton Nothing
      else
        projectLots (concretization cs0 p1) pi1
      where
        tsubproj :: FOnionSub -> PI -> Bool
        tsubproj FSubInt PInt = True
        tsubproj FSubUnit PUnit = True
        tsubproj (FSubLabel sln) (PLabel pln) | sln == pln = True
        tsubproj FSubFun PFun = True
        tsubproj _ _ = False
    (SDEmptyOnion, _) -> Set.singleton Nothing

    (SDFunction _ _ _ _, PFun) -> Set.singleton (Just sd)
    (SDFunction _ _ _ _, _) -> Set.singleton Nothing

    (SDBadness, _ ) -> Set.singleton Nothing

  where
    projectLots :: (Set SDown) -> PI -> Set (Maybe SDown)
    projectLots ss0 pin = Set.fold (Set.union) Set.empty $ Set.fromList
      (let ss1 = Set.toList ss0 in
            (do
                    s <- ss1
                    return $ project cs0 s pin))
-- Find all the possible concretizations of a given starting point
concretization :: Constraints -> ProgramPoint -> Set SDown
concretization cs0 p0 =
  let ps = fst (until (\ ( _ , new) -> new == Set.empty)
        (collectPs) (Set.singleton p0, Set.singleton p0))
        in
  Set.fold (Set.union) (Set.empty) $ Set.map (findSDowns) ps
  where
    -- have Set, want List :( since Set pretends not to be a monad
    cs = Set.toList(cs0)

    -- given an (old,new) pair of sets, produce a new one
    collectPs :: (Set ProgramPoint, Set ProgramPoint) -> (Set ProgramPoint, Set ProgramPoint)
    collectPs (ps0, ps1) =
      let ps' = Set.fold (Set.union) (Set.empty)
            (Set.map (concretization_step) ps1) in
      (ps0 `Set.union` ps', ps' Set.\\ ps0)

    -- Find all the SDowns flowingTo the given ProgramPoint
    findSDowns :: ProgramPoint -> Set SDown
    findSDowns p = Set.fromList $
      do
        SLower sd p' <- cs
        guard (p' == p)
        return sd

    -- Find all the program points that are "one step" from the given one
    concretization_step :: ProgramPoint -> Set ProgramPoint
    concretization_step p = Set.fromList $
      do
        SIntermediate p2 p1 <- cs
        guard (p1 == p)
        return p2

retrieve :: ProgramPoint -> Constraints -> Value
retrieve p cs =
  case fold (find p) Nothing cs of
    Nothing -> error "Could not retrieve"
    Just sd -> case sd of
      SDUnit -> VPrimUnit
      SDInt i -> VPrimInt i
      SDLabel ln p1 -> VLabel ln (retrieve p1 cs)
      SDOnion p1 p2 -> VOnion (retrieve p1 cs) (retrieve p2 cs)
      SDOnionSub p1 s1 -> error $ "Subtration Onion: " ++ (show p1) ++ show s1
        --TODO
      SDEmptyOnion -> VEmptyOnion
      SDFunction ps p1 p2 f -> error $ "Function...what do I do..."
          ++ show ps ++ show p1 ++ show p2 ++ show f --TODO
      SDBadness -> error "Lightening"

  where
    find :: ProgramPoint -> Constraint -> Maybe SDown -> Maybe SDown
    find p1 (SLower sd p2) Nothing | (p1 == p2) = Just sd
    find _ _ Nothing = Nothing
    find _ _ mc = mc
