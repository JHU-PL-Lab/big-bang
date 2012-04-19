{-# LANGUAGE FunctionalDependencies 
           , MultiParamTypeClasses    
           , TypeSynonymInstances
           , UndecidableInstances
           , GADTs                   #-}

module Language.LittleBang.Translator
( convTiny
, convertLittleToTiny
) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad ((>=>))
import Data.Map (Map)
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
  convTiny e =
    case e of
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
      -- TODO: Encoding for cases (to correctly pass self)
      LA.Case e' brs -> TA.Case <$> convTiny e' <*> convTiny brs
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
        -- TODO: this is incorrect; it doesn't do self-binding
        (e1',i') <- convTiny (e1,i)
        free <- next
        return $ TA.Case e1'
          [ TA.Branch (TA.ChiTopBind $ TA.ChiUnbound $
                         TA.ChiLabelShallow (itl i') free) $
                           TA.Var free ]
      LA.ProjAssign e1 i e2 e3 -> do
        (e1',e2',e3',i') <- convTiny (e1,e2,e3,i)
        free <- next
        return $ TA.Case e1'
          [ TA.Branch (TA.ChiTopBind $ TA.ChiUnbound $
                         TA.ChiLabelShallow (itl i') $ free) $
                           TA.Assign (TA.AIdent free) e2' e3' ]
    where self = TUT.ident "self"
          prior = TUT.ident "prior"
          ref = TUT.labelName "Ref"
          itl = TUT.labelName . TUT.unIdent
          selfEncodeBranch :: TA.Branch -> TA.Branch
          selfEncodeBranch = error "TODO: selfEncodeBranch"
          -- |Given a pattern, this function produces a new pattern with binders
          --  at each level.  It then returns a mapping from each pre-existing
          --  binder to the binder representing the prior binder's self.
          selfEncodePattern :: TA.Chi a -> (TA.Chi a, Map TUT.Ident TUT.Ident)
          selfEncodePattern p = error "TODO: selfEncodePattern"

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

