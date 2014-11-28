{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Language.TinyBang.Ast.WellFormedness
( checkWellFormed
, IllFormedness(..)
) where

import Control.Monad
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Trans.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.TinyBang.Ast.Data
import Language.TinyBang.Ast.Origin
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

-- |A data structure representing types of ill-formed expressions.
data IllFormedness
  = DuplicateDefinition Var
      -- ^Generated when a variable is declared twice.
  | OpenExpression (Set Var)
      -- ^Generated when the expression is not closed.
  | OpenPattern (Set Var)
      -- ^Generated when a pattern is not closed.
  | EmptyExpression Origin
      -- ^Generated when an empty expression (an expression with no clauses) is
      --  encountered
  | RefPatternNotExactlyEmpty Var
      -- ^Generated when a ref pattern has anything other than an empty
      --  pattern beneath it.
  deriving (Eq, Ord, Show)

instance Display IllFormedness where
  makeDoc ill = case ill of
    DuplicateDefinition x -> text "Duplicate definition:" <+> makeDoc x
    OpenExpression xs -> text "Expression is open in:" <+> makeDoc xs
    OpenPattern xs -> text "Pattern is open in:" <+> makeDoc xs
    EmptyExpression o -> text "Empty expression at:" <+> makeDoc o
    RefPatternNotExactlyEmpty x ->
      text "Ref pattern not empty at:" <+> makeDoc x

checkWellFormed :: Expr -> Set IllFormedness
checkWellFormed e =
  let (out,ills,varCounts) = runWellFormedM executeChecks in
  let dupeVars =
        filter (\x -> snd x > 1) $ Map.toList $ unVarCountMap varCounts in
  Set.unions
    [ ills
    , either id (const Set.empty) out
    , Set.fromList $ map (DuplicateDefinition . fst) dupeVars
    ]
  where
    executeChecks :: WellFormedM ()
    executeChecks = do
      (_,free) <- boundAndFreeVarsOfExpr e
      unless (Set.null free) $
        reportIllFormednesses $ Set.singleton $ OpenExpression free

-- |Defines a monoid for a variable counting map which merges by addition.
newtype VarCountMap = VarCountMap { unVarCountMap :: Map Var Int }

instance Monoid VarCountMap where
  mempty = VarCountMap Map.empty
  mappend (VarCountMap x) (VarCountMap y) =
    VarCountMap $ Map.unionWith (+) x y
  mconcat xs =
    VarCountMap $ Map.unionsWith (+) $ map unVarCountMap xs

-- |Defines a monad in which well-formedness checks are executed.
newtype WellFormedM a
  = WellFormedM { unWellFormedM ::
                    EitherT (Set IllFormedness)
                      (Writer (Set IllFormedness, VarCountMap))
                        a }
  deriving (Monad, MonadWriter (Set IllFormedness, VarCountMap))
  
runWellFormedM :: WellFormedM a
               -> (Either (Set IllFormedness) a, Set IllFormedness, VarCountMap)
runWellFormedM x =
  let (result, written) = runWriter $ runEitherT $ unWellFormedM x in
  let (reportedIlls, bindCounts) = written in
  (result, reportedIlls, bindCounts)

reportVariablesBound :: Set Var -> WellFormedM ()
reportVariablesBound xs =
  tell (Set.empty, VarCountMap $ Map.fromList $ map (,1) $ Set.toList xs)

reportIllFormednesses :: Set IllFormedness -> WellFormedM ()
reportIllFormednesses ills = tell (ills,mempty)

haltOnIllFormednesses :: Set IllFormedness -> WellFormedM a
haltOnIllFormednesses ills = WellFormedM $ left ills

-- |Determines which variables are bound by a given expression and which are
--  free.  In the process, gathers well-formedness complaints.
boundAndFreeVarsOfExpr :: Expr -> WellFormedM (Set Var, Set Var)
boundAndFreeVarsOfExpr (Expr o cls) =
  if null cls
    then haltOnIllFormednesses $ Set.singleton $ EmptyExpression o
    else do
      (bound,free) <- foldM accumulateBoundAndFreeVarsOfClause
                        (Set.empty, Set.empty) cls
      reportVariablesBound bound
      return (bound,free)
  where
    accumulateBoundAndFreeVarsOfClause :: (Set Var, Set Var)
                                       -> Clause
                                       -> WellFormedM (Set Var, Set Var)
    accumulateBoundAndFreeVarsOfClause (bound,free) cl = do
      clauseBound <- boundVarsOfClause cl
      clauseFree <- freeVarsOfClause cl
      let allBound = Set.union bound clauseBound
      return ( allBound
             , free `Set.union` (clauseFree Set.\\ allBound)
             )

-- |Determines the bound variables of a given clause.
boundVarsOfClause :: Clause -> WellFormedM (Set Var)
boundVarsOfClause (Clause _ x _) = return $ Set.singleton x

-- |Determines the free variables of a given clause.
freeVarsOfClause :: Clause -> WellFormedM (Set Var)
freeVarsOfClause (Clause _ _ r) =
  case r of
    Def _ v -> freeVarsOfValue v
    Copy _ x -> return $ Set.singleton x
    Appl _ x1 x2 -> return $ Set.fromList [x1,x2]
    Builtin _ _ xs -> return $ Set.fromList xs

-- |Determines the free variables appearing in a value.
freeVarsOfValue :: Value -> WellFormedM (Set Var)
freeVarsOfValue v = case v of
  VPrimitive _ _ -> return Set.empty
  VEmptyOnion _ -> return Set.empty
  VLabel _ _ x -> return $ Set.singleton x
  VRef _ x -> return $ Set.singleton x
  VOnion _ x1 x2 -> return $ Set.fromList [x1,x2]
  VScape _ p e -> do
    bafExpr <- boundAndFreeVarsOfExpr e
    bafPat <- boundAndFreeVarsOfPattern p
    return $ snd bafExpr Set.\\ fst bafPat

-- |Determines the bound and free variables appearing within a given pattern.
boundAndFreeVarsOfPattern :: Pattern -> WellFormedM (Set Var, Set Var)
boundAndFreeVarsOfPattern (Pattern _ x'' (PatternFilterMap pfm)) = do
  (bound,free) <- bafAbsF x''
  reportVariablesBound bound
  unless (Set.null free) $
    reportIllFormednesses $ Set.singleton $ OpenPattern free
  return (bound,free)
  where
    bafAbsF :: Var -> WellFormedM (Set Var, Set Var)
    bafAbsF x =
      postLogM _debugI (
        \(bound, free) ->
          display $ text "Pattern" <+> makeDoc x <+> char '\\' <+>
                      makeDoc pfm <+> text "has bound" <+> makeDoc bound <+>
                      text "and has free" <+> makeDoc free
      ) $
      case Map.lookup x pfm of
        Nothing -> return (Set.empty, Set.singleton x)
        Just (_,filt) -> do
          subBaf <- bafConF filt
          let bound = Set.insert x $ fst subBaf
          let free = snd subBaf Set.\\ bound
          return (bound, free)
    bafConF :: Filter -> WellFormedM (Set Var, Set Var)
    bafConF pf =
      postLogM _debugI (
        \(bound, free) ->
          display $ text "Pattern" <+> makeDoc pf <+> char '\\' <+>
                      makeDoc pfm <+> text "has bound" <+> makeDoc bound <+>
                      text "and has free" <+> makeDoc free
      ) $
      case pf of
        FPrimitive _ _ -> return (Set.empty, Set.empty)
        FEmptyOnion _ -> return (Set.empty, Set.empty)
        FLabel _ _ x -> bafAbsF x
        FRef _ x -> do
          confirmOnlyEmptyPattern x
          bafAbsF x
        FConjunction _ x1 x2 -> do
          baf1 <- bafAbsF x1
          baf2 <- bafAbsF x2
          return ( fst baf1 `Set.union` fst baf2
                 , snd baf1 `Set.union` snd baf2
                 )
    confirmOnlyEmptyPattern :: Var -> WellFormedM ()
    confirmOnlyEmptyPattern x =      
      let bad = case Map.lookup x pfm of
                  Nothing -> True
                  Just (_,filt) -> case filt of
                              FEmptyOnion _ -> False
                              _ -> True
      in
      when bad $
        reportIllFormednesses $ Set.singleton $ RefPatternNotExactlyEmpty x
