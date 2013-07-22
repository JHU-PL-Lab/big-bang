{- | 
  This module performs ATranslation to create a TinyBang Expr from a TinyBangNested Expr
-} 

module ATranslation.Translator
(performTranslation)
where

import qualified Language.TinyBang.Ast.Data as TBA
import qualified Language.TinyBangNested.Ast.Data as TBN
import ATranslation.TranslationState
import Language.TinyBang.Syntax.Location
import Control.Monad.State
import Data.Map 
import Data.Maybe

-- | Translation State Monad
type TransM = State TranslationState

type TExprValue = ([TBA.Clause], TBA.FlowVar)

-- | Utility methods for ATranslation

getFreshFlowVar :: TransM TBA.FlowVar
getFreshFlowVar = 
   do myState <- get
      modify incrementFlowVarCount
      return (TBA.FlowVar testOrigin ('x' : show (flowVarCount myState)))
     
getFreshCellVar :: TransM TBA.CellVar
getFreshCellVar = 
   do myState <- get
      modify incrementCellVarCount
      return (TBA.CellVar testOrigin ('y' : show (cellVarCount myState)))
     
-- TODO Use Maybe TBA.CellVar, move error message into aTransform for context specific messages
cellVarLookup :: String -> TransM TBA.CellVar
cellVarLookup varName = 
  do myState <- get
     if isNothing $ lookUp $ varMap myState
       then error $ "Variable undefined: " ++ show varName
       else return $ fromJust $ lookUp $ varMap myState
         where lookUp = Data.Map.lookup varName


-- | Translation for expressions
               
-- TODO fix origins
-- | aTransformExpr recurses over a TBN.Expr, updating the TransM state monad as it goes
-- | with the resulting [clause] in the state representing the TBA.Expr translation.
aTransformExpr :: TBN.Expr -> TransM TExprValue
aTransformExpr expr =
  case expr of
    TBN.ExprDef org (TBN.Var _ varName) e1 e2 ->
      do (varValueCls, varClsFlow) <- aTransformExpr e1
         cellForDefVar <- getFreshCellVar
         let varDefCls =  [genClauseCellDef org cellForDefVar varClsFlow]
         modify $ insertVar varName cellForDefVar
         (exprValueCls, _) <- aTransformExpr e2
         return (varValueCls ++ varDefCls ++ exprValueCls, varClsFlow)
         
    TBN.ExprVarIn org (TBN.Var _ varName) e1 e2 ->
      do (varValueCls, varClsFlow) <- aTransformExpr e1
         cellForSetVar <- cellVarLookup varName
         let varSetCls = [genClauseCellSet org cellForSetVar varClsFlow]
         (exprValueCls, _) <- aTransformExpr e2
         return (varValueCls ++ varSetCls ++ exprValueCls, varClsFlow)
         
    TBN.ExprScape org pattern e ->
      do p <-  aTransformOuterPattern pattern
         (cls, _) <- aTransformExpr e
         freshFlow <- getFreshFlowVar
         let scapeExpr = TBA.Expr org cls 
         let scapeClause = [genClauseScape org freshFlow p scapeExpr]
         return (scapeClause, freshFlow)
         
    TBN.ExprBinaryOp org e1 op e2 -> 
      do (leftCls, leftFlow) <- aTransformExpr e1 
         (rightCls, rightFlow) <- aTransformExpr e2
         freshFlow <- getFreshFlowVar
         return (leftCls ++ rightCls ++ [genClauseBinOp org freshFlow leftFlow op rightFlow]
                , freshFlow)
           
    TBN.ExprOnionOp org e op proj -> 
      do (cls, v) <- aTransformExpr e
         freshFlow <- getFreshFlowVar
         return (cls ++ [genClauseValueDef org freshFlow (genOnionFilterValue org v op proj)]
                , freshFlow)
           
    TBN.ExprOnion org e1 e2 -> 
      do (leftCls, leftFlow) <- aTransformExpr e1 
         (rightCls, rightFlow) <- aTransformExpr e2
         freshFlow <- getFreshFlowVar
         return (leftCls ++ rightCls ++ [genClauseOnion org freshFlow leftFlow rightFlow]
                , freshFlow)
           
    TBN.ExprAppl org e1 e2 -> 
      do (leftCls, leftFlow) <- aTransformExpr e1
         (rightCls, rightFlow) <- aTransformExpr e2
         freshFlow <- getFreshFlowVar
         return (leftCls ++ rightCls ++ [genClauseAppl org freshFlow leftFlow rightFlow]
                , freshFlow)
           
    TBN.ExprLabelExp org l e1 ->
      do (cls, v) <- aTransformExpr e1
         freshFlow <- getFreshFlowVar
         freshCell <- getFreshCellVar
         return (cls ++ [ genClauseCellDef org freshCell v
                        , genClauseValueDef org freshFlow (genLabelValue l freshCell)
                        ]
                , freshFlow)
  
    TBN.ExprVar org (TBN.Var _ varName) -> 
      do freshFlow <- getFreshFlowVar
         cellVar <- cellVarLookup varName
         return ([genClauseCellGet org freshFlow cellVar], freshFlow)
         
    TBN.ExprValInt org i ->
      do freshFlow <- getFreshFlowVar
         return ([genClauseValueDef org freshFlow (TBA.VInt org i)], freshFlow)
         
    TBN.ExprValChar org c ->
      do freshFlow <- getFreshFlowVar
         return ([genClauseValueDef org freshFlow (TBA.VChar org c)], freshFlow)
  
    TBN.ExprValUnit org ->
      do freshFlow <- getFreshFlowVar
         return ([genClauseValueDef org freshFlow (TBA.VEmptyOnion org)], freshFlow)



-- | Translation for patterns
         
-- OuterPattern ::= Var : Pattern
aTransformOuterPattern :: TBN.OuterPattern -> TransM TBA.Pattern
aTransformOuterPattern pat =
  case pat of
    TBN.OuterPatternLabel org (TBN.Var _ varName) innerpat -> 
      do { cellVar <- getFreshCellVar
         ; modify $ insertVar varName cellVar                   
         ; transformedPat <- aTransformInnerPattern innerpat
         ; return (TBA.ValuePattern org cellVar transformedPat)
         }

-- Pattern ::= Pattern & Pattern
-- Pattern ::= Label Var : Pattern
-- Pattern ::= Primitive
-- Pattern ::= fun
-- Pattern ::= ()
aTransformInnerPattern :: TBN.Pattern -> TransM TBA.InnerPattern
aTransformInnerPattern pat =
  case pat of
    TBN.ConjunctionPattern org p1 p2 ->
      do { innerPat1 <- aTransformInnerPattern p1
         ; innerPat2 <- aTransformInnerPattern p2
         ; return $ TBA.ConjunctionPattern org innerPat1 innerPat2
         }    
      
    TBN.LabelPattern org (TBN.LabelDef labelOrg str) (TBN.Var _ varName) pattern ->
      do { cellVar <- getFreshCellVar
         ; modify $ insertVar varName cellVar
         ; innerPat <- aTransformInnerPattern pattern
         ; return $ TBA.LabelPattern org (TBA.LabelName labelOrg str) cellVar innerPat
         }
      
    TBN.PrimitivePattern org prim -> 
      return (TBA.PrimitivePattern org $ primType prim)
        where primType ::TBN.Primitive -> TBA.PrimitiveType
              primType p = 
                case p of
                  TBN.TInt o -> TBA.PrimInt o
                  TBN.TChar o -> TBA.PrimChar o

    TBN.ScapePattern org ->
      return (TBA.ScapePattern org)
      
    TBN.EmptyOnionPattern org ->
      return (TBA.EmptyOnionPattern org)



-- | Generators for TBA Clause types
        
genClauseBinOp :: TBA.Origin -> TBA.FlowVar -> TBA.FlowVar 
               -> TBN.BinaryOperator -> TBA.FlowVar  -> TBA.Clause
genClauseBinOp org flow left op right = 
  TBA.RedexDef org flow (TBA.BinOp org left binop right)
    where binop = 
           case op of
             TBN.OpPlus o -> TBA.OpPlus o
             TBN.OpMinus o -> TBA.OpMinus o
             TBN.OpEqual o -> TBA.OpEqual o
             TBN.OpLesser o -> TBA.OpLess o  
             TBN.OpGreater o -> TBA.OpGreater o
             _ -> error "Interpreter does not support >= or <="

genClauseValueDef :: TBA.Origin -> TBA.FlowVar -> TBA.Value -> TBA.Clause
genClauseValueDef org flow value = TBA.Evaluated (TBA.ValueDef org flow value)
        
genClauseOnion :: TBA.Origin -> TBA.FlowVar -> TBA.FlowVar -> TBA.FlowVar -> TBA.Clause
genClauseOnion org fnew f1 f2 = genClauseValueDef org fnew (TBA.VOnion org f1 f2)

genClauseScape :: TBA.Origin -> TBA.FlowVar -> TBA.Pattern -> TBA.Expr -> TBA.Clause
genClauseScape org flow pat expr = genClauseValueDef org flow (TBA.VScape org pat expr)

genClauseAppl :: TBA.Origin -> TBA.FlowVar -> TBA.FlowVar -> TBA.FlowVar -> TBA.Clause
genClauseAppl org flow f1 f2 = TBA.RedexDef org flow (TBA.Appl org f1 f2)

genClauseCellGet :: TBA.Origin  -> TBA.FlowVar -> TBA.CellVar -> TBA.Clause
genClauseCellGet = TBA.CellGet

genClauseCellSet :: TBA.Origin -> TBA.CellVar -> TBA.FlowVar -> TBA.Clause
genClauseCellSet  = TBA.CellSet

genClauseCellDef :: TBA.Origin -> TBA.CellVar -> TBA.FlowVar -> TBA.Clause
genClauseCellDef org cell flow  = TBA.Evaluated (TBA.CellDef org (TBA.QualNone org) cell flow)

genLabelValue :: TBN.Label -> TBA.CellVar -> TBA.Value 
genLabelValue (TBN.LabelDef o s) cell =  TBA.VLabel o (TBA.LabelName o s) cell 

genOnionFilterValue :: TBA.Origin -> TBA.FlowVar -> TBN.OnionOperator -> TBN.Projector -> TBA.Value
genOnionFilterValue org v op proj = 
     TBA.VOnionFilter org v convertOp convertProj
       where
         convertOp :: TBA.OnionOp
         convertOp = case op of 
           TBN.OpOnionSub o -> TBA.OpOnionSub o
           TBN.OpOnionProj o -> TBA.OpOnionProj o
         convertProj :: TBA.AnyProjector
         convertProj = case proj of
           TBN.PrimitiveProjector o p -> TBA.SomeProjector $ TBA.ProjPrim o (convertPrim p)
           TBN.LabelProjector o (TBN.LabelDef labelOrg s) -> 
             TBA.SomeProjector $ TBA.ProjLabel o (TBA.LabelName labelOrg s)
           TBN.FunProjector o -> TBA.SomeProjector $ TBA.ProjFun o
         convertPrim :: TBN.Primitive -> TBA.PrimitiveType
         convertPrim p = case p of
           TBN.TInt o -> TBA.PrimInt o
           TBN.TChar o -> TBA.PrimChar o        

-- | Setup state for performTransformation:

testLocation :: SourceRegion
testLocation = SourceRegion Unknown Unknown

testOrigin :: TBA.Origin
testOrigin = TBA.SourceOrigin testLocation

startState :: TranslationState
startState = TranslationState 0 0 empty

performTranslation :: TBN.Expr -> TBA.Expr
performTranslation expr = TBA.Expr testOrigin $ fst $ evalState (aTransformExpr expr) startState
