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
         ,LA.ExprPart :<< ast1
         ,TA.ExprPart :<< ast1
         ,TA.ExprPart :<< ast2)
      => XvPart EncodeLittleBangOp LA.ExprPart ast1 (LBEnc ast1 ast2)
  where
  xvPart EncodeLittleBangOp part = \f ->
    case part of
      LA.Prior -> return $ TA.var prior
      LA.Self -> return $ TA.var self
      LA.Proj e i -> do
        -- o.x ==> (`x x -> x) o
        e' <- f e
        let pat = TA.Pattern (TUT.ident "_") $ TA.PatLabel (itl i) i $
                    TA.PatOnion []
        return $ TA.appl (TA.scape pat $ TA.var i) e'
      LA.ProjAssign e1 i e2 e3 -> do
        -- o.x = e in e' ==> (`x x -> x = e in e') o
        e1' <- f e1
        e2' <- f e2
        e3' <- f e3
        free <- next
        let pat = TA.Pattern (TUT.ident "_") $ TA.PatLabel (itl i) free $
                    TA.PatOnion []
        return $ TA.appl (TA.scape pat $ TA.assign free e2' e3') e1'
      LA.Onion e1 e2 -> do
        -- e1 & e2 ==> (prior -> (prior -> prior & e2) e1) (&)
        e1' <- f e1
        e2' <- f e2
        return $ TA.appl (TA.scape (varp prior) $
                    TA.appl (TA.scape (varp prior) $
                        TA.onion (TA.var prior) e2'
                        ) $ e1'
                    ) $ TA.emptyOnion
      LA.Scape (TA.Pattern i p) e -> do
        -- (p -> e) ==> (p & `self self -> e)
        e' <- f e
        let p' = TA.PatOnion [p,
                    TA.PatLabel (itl self) self $ TA.PatOnion []]
        return $ TA.scape (TA.Pattern i p') e'
      LA.Appl e1 e2 -> do
        -- e1 e2 ==> (f -> f (e2 & `self f)) e1
        e1' <- f e1
        e2' <- f e2
        free <- next
        return $ TA.appl (TA.scape (varp free) $
                    TA.appl (TA.var free) $
                        TA.onion e2' $
                            TA.label (itl self) (Just TA.Final) $ TA.var free
                    ) $ e1'
    where prior = TUT.ident "prior"
          self = TUT.ident "self"
          itl = TUT.labelName . TUT.unIdent
          varp :: TUT.Ident -> TA.Pattern
          varp ident = TA.Pattern ident $ TA.PatOnion []

