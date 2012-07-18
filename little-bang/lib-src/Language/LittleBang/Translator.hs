{-# LANGUAGE FunctionalDependencies 
           , MultiParamTypeClasses    
           , TypeSynonymInstances
           , UndecidableInstances
           , FlexibleInstances
           , FlexibleContexts
           , ScopedTypeVariables
           , GADTs
           #-}

module Language.LittleBang.Translator
( convertLittleToTiny
) where

import Data.Foldable (foldlM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Consumer (Consumer, evalConsumer, next)

import qualified Language.LittleBang.Ast as LA
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Types.UtilTypes as TUT

-- import Utils.Render.Display
import Data.ExtensibleVariant

type FreshVars = Consumer TUT.Ident

-- |Converts a LittleBang AST to a TinyBang AST.
convertLittleToTiny :: LA.Expr -> TA.Expr
convertLittleToTiny e = 
  let names = map (("freshTmp" ++ ) . show) [(0::Integer)..] in
  let used = Set.map TUT.unIdent $ TA.exprVars e in
  let freshIdents = map TUT.ident $ filter (not . (`Set.member` used)) names in
  let ce = convTiny e in
  evalConsumer ce freshIdents

-- |Converts a LittleBang AST to a TinyBang AST in the presence of an
--  environment providing free variables.
-- NOTE: due to a bug in GHC 7.4.1 at the time of this writing, convTiny cannot
-- have a declared type signature.  For more information, see:
--     http://hackage.haskell.org/trac/ghc/ticket/6065
--convTiny :: (XvOp EncodeLittleBangOp ast1 (LBEnc ast1 ast2))
--         => ast1 -> FreshVars ast2
convTiny e = xvOp EncodeLittleBangOp e convTiny

-- |Represents an operation for translating LittleBang AST nodes into TinyBang
--  AST nodes.
data EncodeLittleBangOp = EncodeLittleBangOp

-- |A type synonym describing the result type of the LittleBang encoding
--  operation.
type LBEnc ast1 ast2 = (ast1 -> FreshVars ast2) -> FreshVars ast2

-- |Performs "encoding" for non-LittleBang ASTs.  This is required to satisfy
--  the homomorphic properties of non-LittleBang nodes.
instance (XvPart HomOpM TA.ExprPart ast1 (LBEnc ast1 ast2))
      => XvPart EncodeLittleBangOp TA.ExprPart ast1 (LBEnc ast1 ast2) where
  xvPart EncodeLittleBangOp p = \f -> xvPart HomOpM p f

-- |Defines an operation for converting LittleBang AST nodes into TinyBang AST
--  nodes.
instance (XvOp EncodeLittleBangOp ast1 (LBEnc ast1 ast2)
         ,XvOp TA.SubstOp ast2 (TA.ExprPart ast2 -> TUT.Ident -> ast2)
         ,LA.ExprPart :<< ast1
         ,TA.ExprPart :<< ast1
         ,TA.ExprPart :<< ast2)
      => XvPart EncodeLittleBangOp LA.ExprPart ast1 (LBEnc ast1 ast2)
  where
  xvPart EncodeLittleBangOp part = \f ->
    case part of
      LA.Prior -> return $ inj $ TA.Var $ prior
      LA.Self -> return $ inj $ TA.Var $ self
      LA.Proj e i -> do
        -- Because this is fairly complex, we're going to convert to a
        -- LittleBang case expression and then recurse.
        free <- next
        let caseExpr :: ast1
            caseExpr = inj $ LA.Case e
                [ LA.Branch (LA.ChiTopBind $ LA.ChiUnbound $
                        LA.ChiLabelShallow (itl i) free) $
                            inj $ TA.Var free ]
        (convTiny caseExpr)::(FreshVars ast2)
      LA.ProjAssign e1 i e2 e3 -> do
        -- Because TinyBang self encoding doesn't affect assignment
        -- expressions, this is fairly simple.
        e1' <- f e1
        e2' <- f e2
        e3' <- f e3
        free <- next
        return $ inj $ TA.Case e1'
          [ TA.Branch (TA.ChiTopBind $ TA.ChiUnbound $
                        TA.ChiLabelShallow (itl i) $ free) $ inj $
                          TA.Assign free e2' e3' ]
      LA.Case e brs -> do
        e' <- f e
        brs' <- mapM (selfEncodeBranch f) brs
        return $ inj $ TA.Case e' brs'
      LA.Onion e1 e2 -> do
        e1' <- f e1
        e2' <- f e2
        return $ inj $ TA.Case (inj $ TA.EmptyOnion)
            [ TA.Branch (TA.ChiTopVar prior) $ inj $
                TA.Case e1' [ TA.Branch (TA.ChiTopVar prior) $ inj $ 
                                TA.Onion (inj $ TA.Var prior) e2' ] ]
      LA.Func i e -> do
        e' <- f e
        return $ inj $ TA.Func self $ inj $ TA.Func i $ e'
      LA.Appl e1 e2 -> do
        e1' <- f e1
        e2' <- f e2
        return $ inj $ TA.Appl
            (inj $ TA.Appl e1' $ inj $ TA.EmptyOnion) e2'
    where prior = TUT.ident "prior"
          self = TUT.ident "self"
          itl = TUT.labelName . TUT.unIdent
          selfEncodeBranch :: (ast1 -> FreshVars ast2)
                           -> TA.Branch ast1
                           -> FreshVars (TA.Branch ast2)
          selfEncodeBranch f (TA.Branch pat bexpr) = do
            bexpr' <- f bexpr
            npat <- fullyNamePattern pat
            bexpr'' <- foldlM exprFuncSubst bexpr' $ Map.toList $
                assignPatternSelfNames npat
            return $ TA.Branch npat bexpr''
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
          -- |Given an expression, replaces each instance of the variable
          --  @var@ with a self-encoding pattern using @vself@ as the
          --  self-variable.
          exprFuncSubst :: ast2 -> (TUT.Ident, TUT.Ident) -> FreshVars ast2
          exprFuncSubst sexpr (var,vself) = do
            junk <- next
            junk2 <- next
            let repl = TA.Case (inj $ TA.Var var) $
                        [ TA.Branch (TA.ChiTopBind $ TA.ChiUnbound $
                          TA.ChiFun) $ inj $ TA.Func junk $ inj $
                          TA.Appl (inj $ TA.Var var) $ inj $
                          TA.Var vself
                        , TA.Branch (TA.ChiTopVar junk2) $ inj $
                          TA.Var var ]
            return $ TA.subst sexpr repl var