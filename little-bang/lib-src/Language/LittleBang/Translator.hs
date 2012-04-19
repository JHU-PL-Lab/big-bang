{-# LANGUAGE FunctionalDependencies 
           , MultiParamTypeClasses    
           , TypeSynonymInstances
           , UndecidableInstances
           , GADTs                   #-}

module Language.LittleBang.Translator
( convTiny
, convertLittleToTiny
) where

import Control.Applicative ((<$>),(<*>),pure)
import Control.Monad ((>=>))
import Data.Foldable (foldlM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Consumer (Consumer, evalConsumer, next)

import qualified Language.LittleBang.Ast as LA
import qualified Language.LittleBang.Types.UtilTypes as LUT
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Types.UtilTypes as TUT

-- import Utils.Render.Display

type FreshVars = Consumer TUT.Ident

-- |Converts a LittleBang AST to a TinyBang AST.
convertLittleToTiny :: LA.Expr -> TA.Expr
convertLittleToTiny e =
  let names = map (("freshTmp" ++ ) . show) [(0::Integer)..] in
  let used = Set.map LUT.unIdent $ LA.exprVars e in
  let freshIdents = map TUT.ident $ filter (not . (`Set.member` used)) names in
  evalConsumer ((convTiny e)::(FreshVars TA.Expr)) freshIdents

-- |Translates a Little Bang AST to a Tiny Bang AST.
class ConvertibleToTinyBang a b | a -> b where
  convTiny :: a -> FreshVars b

instance (ConvertibleToTinyBang a b) => ConvertibleToTinyBang [a] [b] where
  convTiny = mapM convTiny

instance (ConvertibleToTinyBang a b) =>
         ConvertibleToTinyBang (Maybe a) (Maybe b) where
  convTiny = maybe (return Nothing) (convTiny >=> return . Just)

instance (ConvertibleToTinyBang a1 b1, ConvertibleToTinyBang a2 b2) =>
         ConvertibleToTinyBang (a1,a2) (b1,b2) where
  convTiny (x,y) = (,) <$> convTiny x <*> convTiny y

instance (ConvertibleToTinyBang a1 b1, ConvertibleToTinyBang a2 b2,
          ConvertibleToTinyBang a3 b3) =>
         ConvertibleToTinyBang (a1,a2,a3) (b1,b2,b3) where
  convTiny (x,y,z) = (,,) <$> convTiny x <*> convTiny y <*> convTiny z

instance (ConvertibleToTinyBang a1 b1, ConvertibleToTinyBang a2 b2,
          ConvertibleToTinyBang a3 b3, ConvertibleToTinyBang a4 b4) =>
         ConvertibleToTinyBang (a1,a2,a3,a4) (b1,b2,b3,b4) where
  convTiny (w,x,y,z) = (,,,) <$> convTiny w <*> convTiny x <*> convTiny y
                             <*> convTiny z

instance ConvertibleToTinyBang LUT.LabelName TUT.LabelName where
  convTiny = return . TUT.labelName . LUT.unLabelName

instance ConvertibleToTinyBang LUT.Ident TUT.Ident where
  convTiny = return . TUT.ident . LUT.unIdent

instance ConvertibleToTinyBang LUT.PrimitiveType TUT.PrimitiveType where
  convTiny p =
    return $ case p of
      LUT.PrimInt -> TUT.PrimInt
      LUT.PrimChar -> TUT.PrimChar
      LUT.PrimUnit -> TUT.PrimUnit

instance ConvertibleToTinyBang LUT.ProjTerm TUT.ProjTerm where
  convTiny s =
    case s of
      LUT.ProjPrim p -> TUT.ProjPrim <$> convTiny p
      LUT.ProjLabel n -> TUT.ProjLabel <$> convTiny n
      LUT.ProjFunc -> return TUT.ProjFunc

instance ConvertibleToTinyBang LA.Expr TA.Expr where
  convTiny expr =
    case expr of
      LA.Var i -> TA.Var <$> convTiny i
      LA.Label n m e' -> TA.Label <$> convTiny n <*> convTiny m <*> convTiny e'
      LA.Onion e1 e2 -> do
        (e1',e2') <- convTiny (e1,e2)
        return $ TA.Case TA.EmptyOnion
          [ TA.Branch (TA.ChiTopVar prior) $
              TA.Case e1' [ TA.Branch (TA.ChiTopVar prior) $
                              TA.Onion (TA.Var prior) e2' ] ]
      LA.OnionSub e' p -> TA.OnionSub <$> convTiny e' <*> convTiny p
      LA.OnionProj e' p -> TA.OnionProj <$> convTiny e' <*> convTiny p
      LA.EmptyOnion -> return TA.EmptyOnion
      LA.Func i e' -> TA.Func self <$> (TA.Func <$> convTiny i <*> convTiny e')
      LA.Appl e1 e2 -> do
        (e1',e2') <- convTiny (e1,e2)
        return $ TA.Appl (TA.Appl e1' TA.EmptyOnion) e2'
      LA.PrimInt i -> return $ TA.PrimInt i
      LA.PrimChar c -> return $ TA.PrimChar c
      LA.PrimUnit -> return TA.PrimUnit
      LA.Case e' brs -> TA.Case <$> convTiny e' <*> mapM selfEncodeBranch brs
      LA.Def m i e1 e2 -> do
        (m',i',e1',e2') <- convTiny (m,i,e1,e2)
        return $ TA.Case (TA.Label ref m' e1')
          [ TA.Branch (TA.ChiTopBind $ TA.ChiUnbound $
                         TA.ChiLabelShallow ref i') e2' ]
      LA.Assign i e1 e2 -> do
        (i',e1',e2') <- convTiny (i,e1,e2)
        return $ TA.Assign (TA.AIdent i') e1' e2'
      LA.BinOp op e1 e2 -> do
        (e1',e2') <- convTiny (e1,e2)
        return $ (case op of
            LA.Plus -> TA.LazyOp TA.Plus
            LA.Minus -> TA.LazyOp TA.Minus
            LA.Equal -> TA.EagerOp TA.Equal
            LA.LessEqual -> TA.EagerOp TA.LessEqual
            LA.GreaterEqual -> TA.EagerOp TA.GreaterEqual) e1' e2'
      LA.Self -> return $ TA.Var self
      LA.Prior -> return $ TA.Var prior
      LA.Proj e1 i -> do
        -- We're going to convert to a LittleBang case and then recurse
        free' <- next
        let free = LUT.ident $ TUT.unIdent free'
        let caseExpr = LA.Case e1
              [ LA.Branch (LA.ChiTopBind $ LA.ChiUnbound $
                    LA.ChiLabelShallow (litl i) free) $ LA.Var free ]
        convTiny caseExpr
      LA.ProjAssign e1 i e2 e3 -> do
        -- This one can be converted directly to TinyBang because the self
        -- encoding doesn't affect assignment expressions.
        (e1',e2',e3',i') <- convTiny (e1,e2,e3,i)
        free <- next
        return $ TA.Case e1'
          [ TA.Branch (TA.ChiTopBind $ TA.ChiUnbound $
                         TA.ChiLabelShallow (titl i') $ free) $
                           TA.Assign (TA.AIdent free) e2' e3' ]
    where self = TUT.ident "self"
          prior = TUT.ident "prior"
          ref = TUT.labelName "Ref"
          litl = LUT.labelName . LUT.unIdent
          titl = TUT.labelName . TUT.unIdent
          -- |Given a branch, performs self encoding on that branch.
          selfEncodeBranch :: LA.Branch -> FreshVars TA.Branch
          selfEncodeBranch (LA.Branch pat lbexpr) = do
            (tinypat,bexpr) <- convTiny (pat,lbexpr)
            npat <- fullyNamePattern tinypat
            bexpr' <- foldlM exprFuncSubst bexpr $ Map.toList $
                assignPatternSelfNames npat
            return $ TA.Branch npat bexpr'
            where -- |Substitutes variable uses in an expression with the
                  --  appropriate case expression.  This substitution is
                  --  capture-avoiding.
                  {- TODO: consider moving this to a more general location -}
                  exprFuncSubst :: TA.Expr -> (TUT.Ident,TUT.Ident)
                                -> FreshVars TA.Expr
                  exprFuncSubst se (k,v) = case se of
                    TA.Var i | i == k -> do
                      junk <- next
                      junk2 <- next
                      return $ TA.Case se
                        [ TA.Branch (TA.ChiTopBind $ TA.ChiUnbound $
                          TA.ChiFun) $ TA.Func junk $ TA.Appl se $ TA.Var v
                        , TA.Branch (TA.ChiTopVar junk2) se ]
                    TA.Var _ -> return se
                    TA.Label n m e -> TA.Label n m <$> rec e
                    TA.Onion e1 e2 -> TA.Onion <$> rec e1 <*> rec e2
                    TA.OnionSub e p -> TA.OnionSub <$> rec e <*> pure p
                    TA.OnionProj e p -> TA.OnionProj <$> rec e <*> pure p
                    TA.EmptyOnion -> return se
                    TA.Func i _ | i == k -> return se
                    TA.Func i e -> TA.Func i <$> rec e
                    TA.Appl e1 e2 -> TA.Appl <$> rec e1 <*> rec e2
                    TA.PrimInt _ -> return se
                    TA.PrimChar _ -> return se
                    TA.PrimUnit -> return se
                    TA.Case e brs -> TA.Case <$> rec e <*> mapM substBr brs
                    TA.Def m i e1 e2 | i == k ->
                      TA.Def m i <$> rec e1 <*> pure e2
                    TA.Def m i e1 e2 -> TA.Def m i <$> rec e1 <*> rec e2
                    TA.Assign a e1 e2 -> TA.Assign a <$> rec e1 <*> rec e2
                    TA.LazyOp op e1 e2 -> TA.LazyOp op <$> rec e1 <*> rec e2
                    TA.EagerOp op e1 e2 -> TA.EagerOp op <$> rec e1 <*> rec e2
                    TA.ExprCell _ -> return se
                    where rec x = exprFuncSubst x (k,v)
                          substBr :: TA.Branch -> FreshVars TA.Branch
                          substBr branch@(TA.Branch bipat biexpr) =
                            if Set.member k $ TA.ePatVars bipat
                              then return branch
                              else TA.Branch bipat <$> rec biexpr

          -- |Given a pattern, this function creates a new pattern of the same
          --  structure but with additional binders.  These binders ensure that
          --  every nameable point in the structure is named.
          fullyNamePattern :: TA.Chi a -> FreshVars (TA.Chi a)
          fullyNamePattern pat = case pat of
            TA.ChiTopVar _ -> return pat
            TA.ChiTopOnion p s -> do
              fresh <- next
              return $ TA.ChiTopBind $ TA.ChiBound fresh $ TA.ChiUnbound $
                TA.ChiInnerStruct $ TA.ChiOnionMany p s
            TA.ChiTopBind b -> do
              b' <- fullyNamePattern b
              return $ TA.ChiTopBind b'
            TA.ChiOnionMany p s -> do
              p' <- fullyNamePattern p
              s' <- fullyNamePattern s
              return $ TA.ChiOnionMany p' s'
            TA.ChiOnionOne p -> do
              p' <- fullyNamePattern p
              return $ TA.ChiOnionOne p'
            TA.ChiBound i ib@(TA.ChiBound _ _) -> do
              ib' <- fullyNamePattern ib
              return $ TA.ChiBound i ib'
            TA.ChiBound i (TA.ChiUnbound p) -> do
              p' <- fullyNamePattern p
              return $ TA.ChiBound i $ TA.ChiUnbound p'
            TA.ChiUnbound p -> do
              fresh <- next
              p' <- fullyNamePattern p
              return $ TA.ChiBound fresh $ TA.ChiUnbound p'
            TA.ChiPrim _ -> return pat
            TA.ChiLabelShallow _ _ -> return pat
            TA.ChiLabelDeep n b -> do
              b' <- fullyNamePattern b
              return $ TA.ChiLabelDeep n b'
            TA.ChiFun -> return pat
            TA.ChiInnerStruct s -> do
              s' <- fullyNamePattern s
              return $ TA.ChiInnerStruct s'
          -- |Given a fully-named pattern, this function creates a mapping from
          --  each of the variables in the pattern to that variable's
          --  self-variable; for example, in "z:`A x", z is the self-variable
          --  for x.
          assignPatternSelfNames :: TA.Chi a -> Map TUT.Ident TUT.Ident
          assignPatternSelfNames = rec Nothing
            where rec :: Maybe (TUT.Ident) -> TA.Chi a
                      -> Map TUT.Ident TUT.Ident
                  rec mi pat = case pat of
                    TA.ChiTopVar _ -> Map.empty
                    TA.ChiTopOnion p s -> rec mi p `Map.union` rec mi s
                    TA.ChiTopBind b -> rec mi b
                    TA.ChiOnionMany p s -> rec mi p `Map.union` rec mi s
                    TA.ChiOnionOne p -> rec mi p
                    TA.ChiBound i b -> mapFor i mi `Map.union` rec (Just i) b
                    TA.ChiUnbound p -> rec mi p
                    TA.ChiPrim _ -> Map.empty
                    TA.ChiLabelShallow _ i -> mapFor i mi
                    TA.ChiLabelDeep _ b -> rec mi b
                    TA.ChiFun -> Map.empty
                    TA.ChiInnerStruct s -> rec mi s
                    where mapFor :: TUT.Ident -> Maybe (TUT.Ident)
                                 -> Map TUT.Ident TUT.Ident
                          mapFor i = maybe Map.empty (i `Map.singleton`)

instance ConvertibleToTinyBang LA.Modifier TA.Modifier where
  convTiny m =
    return $ case m of
      LA.Final -> TA.Final
      LA.Immutable -> TA.Immutable

instance ConvertibleToTinyBang LA.Branch TA.Branch where
  convTiny (LA.Branch chi e) =
    TA.Branch <$> convTiny chi <*> convTiny e

instance ConvertibleToTinyBang LA.ChiMain TA.ChiMain where
  convTiny chi =
    case chi of
      LA.ChiTopVar x -> TA.ChiTopVar <$> convTiny x
      LA.ChiTopOnion chiP chiS ->
        TA.ChiTopOnion <$> convTiny chiP <*> convTiny chiS
      LA.ChiTopBind chiB ->
        TA.ChiTopBind <$> convTiny chiB

instance ConvertibleToTinyBang LA.ChiStruct TA.ChiStruct where
  convTiny chi =
    case chi of
      LA.ChiOnionOne chiP -> TA.ChiOnionOne <$> convTiny chiP
      LA.ChiOnionMany chiP chiS ->
        TA.ChiOnionMany <$> convTiny chiP <*> convTiny chiS

instance ConvertibleToTinyBang LA.ChiBind TA.ChiBind where
  convTiny chi =
    case chi of
      LA.ChiBound i chiB -> TA.ChiBound <$> convTiny i <*> convTiny chiB
      LA.ChiUnbound chiP -> TA.ChiUnbound <$> convTiny chiP

instance ConvertibleToTinyBang LA.ChiPrimary TA.ChiPrimary where
  convTiny chi =
    case chi of
      LA.ChiPrim p -> TA.ChiPrim <$> convTiny p
      LA.ChiLabelShallow n i ->
        TA.ChiLabelShallow <$> convTiny n <*> convTiny i
      LA.ChiLabelDeep n chiB ->
        TA.ChiLabelDeep <$> convTiny n <*> convTiny chiB
      LA.ChiFun -> return TA.ChiFun
      LA.ChiInnerStruct chiS -> TA.ChiInnerStruct <$> convTiny chiS

