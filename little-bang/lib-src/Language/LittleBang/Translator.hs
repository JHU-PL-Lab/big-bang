{-# LANGUAGE FunctionalDependencies 
           , MultiParamTypeClasses    
           , TypeSynonymInstances
           , UndecidableInstances
           , GADTs                   #-}

module Language.LittleBang.Translator
( convTiny
) where

import qualified Data.Set as Set

import qualified Language.LittleBang.Ast as LA
import qualified Language.LittleBang.Types.UtilTypes as LUT
import qualified Language.TinyBang.Ast as TA
import qualified Language.TinyBang.Types.UtilTypes as TUT

-- import Utils.Render.Display

-- |Given some Tiny Bang ASTs, generates variables which are not free in any of
--  those ASTs.  This list of variables is guaranteed to be infinite in size.
freshVars :: [ TA.Expr ] -> [ TUT.Ident ]
freshVars es =
  let names = map (TUT.ident . ("freshTmp" ++)) $ map show [(0::Int)..] in
  let free = Set.unions $ map TA.exprFreeVars es in
  let goodNames = filter (not . (`Set.member` free)) names in
  goodNames

-- |Translates a Little Bang AST to a Tiny Bang AST.
class ConvertibleToTinyBang a b | a -> b where
  convTiny :: a -> b

instance (ConvertibleToTinyBang a b) => ConvertibleToTinyBang [a] [b] where
  convTiny = map convTiny

instance (ConvertibleToTinyBang a b) =>
         ConvertibleToTinyBang (Maybe a) (Maybe b) where
  convTiny = maybe Nothing $ Just . convTiny

instance ConvertibleToTinyBang LUT.LabelName TUT.LabelName where
  convTiny = TUT.labelName . LUT.unLabelName

instance ConvertibleToTinyBang LUT.Ident TUT.Ident where
  convTiny = TUT.ident . LUT.unIdent

instance ConvertibleToTinyBang LUT.PrimitiveType TUT.PrimitiveType where
  convTiny p =
    case p of
      LUT.PrimInt -> TUT.PrimInt
      LUT.PrimChar -> TUT.PrimChar
      LUT.PrimUnit -> TUT.PrimUnit

instance ConvertibleToTinyBang LUT.SubTerm TUT.SubTerm where
  convTiny s =
    case s of
      LUT.SubPrim p -> TUT.SubPrim $ convTiny p
      LUT.SubLabel n -> TUT.SubLabel $ convTiny n
      LUT.SubFunc -> TUT.SubFunc

instance ConvertibleToTinyBang LA.Expr TA.Expr where
  convTiny e =
    case e of
      LA.Var i -> TA.Var $ convTiny i
      LA.Label n e' -> TA.Label (convTiny n) (convTiny e')
      LA.Onion e1 e2 ->
        TA.Case TA.EmptyOnion
          [ TA.Branch (TA.ChiTopVar self) $
              TA.Case (convTiny e1)
                [ TA.Branch (TA.ChiTopVar self) $
                    TA.Onion (TA.Var self) $ convTiny e2 ] ]
      LA.OnionSub e' s -> TA.OnionSub (convTiny e') (convTiny s)
      LA.EmptyOnion -> TA.EmptyOnion
      LA.Func i e' -> TA.Func self $ TA.Func (convTiny i) (convTiny e')
      LA.Appl e1 e2 ->
        let (e1',e2') = (convTiny e1, convTiny e2) in
        let free = head $ freshVars [e1',e2'] in
        TA.Case e1'
          [ TA.Branch (TA.ChiTopVar free) $
              TA.Appl (TA.Appl (TA.Var free) (TA.Var free)) e2' ]
      LA.PrimInt i -> TA.PrimInt i
      LA.PrimChar c -> TA.PrimChar c
      LA.PrimUnit -> TA.PrimUnit
      -- TODO: Encoding for cases (to correctly pass self)
      LA.Case e' brs -> TA.Case (convTiny e') (convTiny brs)
      LA.Def i e1 e2 ->
        TA.Case (TA.Label ref $ convTiny e1)
          [ TA.Branch (TA.ChiTopBind $ TA.ChiUnbound $
                         TA.ChiLabelShallow ref $ convTiny i) $
              convTiny e2 ]
      LA.Assign i e1 e2 ->
        TA.Assign (TA.AIdent $ convTiny i) (convTiny e1) (convTiny e2)
      LA.BinOp op e1 e2 ->
        (case op of
            LA.Plus -> TA.LazyOp TA.Plus
            LA.Minus -> TA.LazyOp TA.Minus
            LA.Equal -> TA.EagerOp TA.Equal
            LA.LessEqual -> TA.EagerOp TA.LessEqual
            LA.GreaterEqual -> TA.EagerOp TA.GreaterEqual)
          (convTiny e1) (convTiny e2)
      LA.Self -> TA.Var self
    where self = TUT.ident "self"
          ref = TUT.labelName "Ref"

instance ConvertibleToTinyBang LA.Branch TA.Branch where
  convTiny (LA.Branch chi e) =
    TA.Branch (convTiny chi) (convTiny e)

instance ConvertibleToTinyBang LA.ChiMain TA.ChiMain where
  convTiny chi =
    case chi of
      LA.ChiTopVar x -> TA.ChiTopVar $ convTiny x
      LA.ChiTopOnion chiP chiS ->
        TA.ChiTopOnion (convTiny chiP) (convTiny chiS)
      LA.ChiTopBind chiB ->
        TA.ChiTopBind $ convTiny chiB

instance ConvertibleToTinyBang LA.ChiStruct TA.ChiStruct where
  convTiny chi =
    case chi of
      LA.ChiOnionOne chiP -> TA.ChiOnionOne $ convTiny chiP
      LA.ChiOnionMany chiP chiS ->
        TA.ChiOnionMany (convTiny chiP) (convTiny chiS)

instance ConvertibleToTinyBang LA.ChiBind TA.ChiBind where
  convTiny chi =
    case chi of
      LA.ChiBound i chiB -> TA.ChiBound (convTiny i) (convTiny chiB)
      LA.ChiUnbound chiP -> TA.ChiUnbound $ convTiny chiP

instance ConvertibleToTinyBang LA.ChiPrimary TA.ChiPrimary where
  convTiny chi =
    case chi of
      LA.ChiPrim p -> TA.ChiPrim $ convTiny p
      LA.ChiLabelShallow n i -> TA.ChiLabelShallow (convTiny n) (convTiny i)
      LA.ChiLabelDeep n chiB -> TA.ChiLabelDeep (convTiny n) (convTiny chiB)
      LA.ChiFun -> TA.ChiFun
      LA.ChiInnerStruct chiS -> TA.ChiInnerStruct $ convTiny chiS

