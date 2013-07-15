module ATranslation.Translator
(performTransformation)
where

import qualified Language.TinyBang.Ast.Data as TBA
import qualified Language.TinyBangNested.Ast.Data as TBN
import Language.TinyBang.Syntax.Location
import Control.Monad.State
import Data.Map
import Data.Maybe

type TState = (Integer, Integer, Map String TBA.CellVar)
type TValue = ([TBA.Clause], TBA.FlowVar)
type TransM = State TState


addClause :: TBA.Clause -> TBA.FlowVar -> TransM TValue
addClause clause flowVar = return ([clause], flowVar)
                           
getFreshFlowVar :: TransM TBA.FlowVar
getFreshFlowVar = do { (x, y, m) <- get
                  ; put (x + 1, y, m)
                  ; return (TBA.FlowVar testOrigin ('x' : show x))
                  }
                  
getFreshCellVar :: TransM TBA.CellVar
getFreshCellVar = do { (x, y, m) <- get
                  ; put (x, y + 1, m)
                  ; return (TBA.CellVar testOrigin ('y' : show y))
                  }                 

-- TODO Use Either, move error message into aTransform for context specific messages
cellVarLookup :: String -> TransM TBA.CellVar
cellVarLookup v = do { (_, _, m) <- get
                     ; if ( Data.Map.lookup v m == Nothing ) 
                       then error ("Variable undefined: " ++ show v)
                       else return (fromJust $ Data.Map.lookup v m )
                     }

aTransform :: TBN.Expr -> TransM TValue
aTransform expr =
  case expr of
    TBN.ExprDef org var e1 e2 ->
      do { (varValueCls, varClsFlow) <- aTransform e1 --first do e1, now referenced by varClsFlow
         ; cellForDefVar <- getFreshCellVar -- cell to have value from varClsFlow
         ; (varDefCls, _) <- addClause (genClauseCellDef org cellForDefVar varClsFlow) varClsFlow
         ; (x, y, m) <- get 
         ; put (x, y, insert (getVarName var) cellForDefVar m) --put new cell in map
         ; (exprValueCls, _) <- aTransform e2
         ; return (varValueCls ++ varDefCls ++ exprValueCls, varClsFlow)
         }
    TBN.ExprVarIn org var e1 e2 ->
      do { (varValueCls, varClsFlow) <- aTransform e1 -- first do e1, now referenced by varClsFlow
         ; cellForSetVar <- cellVarLookup $ getVarName var -- lookup var
         ; (varSetCls, _) <- addClause (genClauseCellSet org cellForSetVar varClsFlow) varClsFlow
         ; (exprValueCls, _) <- aTransform e2 -- do e2
         ; return (varValueCls ++ varSetCls ++ exprValueCls, varClsFlow)
         }
    TBN.ExprArithOp org e1 op e2 -> 
      do { (leftCls, leftFlow) <- aTransform e1 
         ; (rightCls, rightFlow) <- aTransform e2
         ; freshFlow <- getFreshFlowVar
         ; return (leftCls ++ rightCls ++ [genClauseBinOp org freshFlow leftFlow op rightFlow], freshFlow)
         }
    TBN.ExprOnion org e1 e2 -> 
      do { (leftCls, leftFlow) <- aTransform e1 
         ; (rightCls, rightFlow) <- aTransform e2
         ; freshFlow <- getFreshFlowVar
         ; return (leftCls ++ rightCls ++ [genClauseOnion org freshFlow leftFlow rightFlow], freshFlow)
         }
    TBN.ExprAppl org e1 e2 -> 
      do { (leftCls, leftFlow) <- aTransform e1 
         ; (rightCls, rightFlow) <- aTransform e2
         ; freshFlow <- getFreshFlowVar
         ; return (leftCls ++ rightCls ++ [genClauseAppl org freshFlow leftFlow rightFlow], freshFlow)
         }
    TBN.ExprLabelExp org l e1 ->
      do { (cls, v) <- aTransform e1
         ; freshFlow <- getFreshFlowVar
         ; freshCell <- getFreshCellVar
         ; return (cls ++ [genClauseCellDef org freshCell v, genClauseValueDef org freshFlow (genLabelValue l freshCell)], freshFlow)
         }
    TBN.ExprValInt org i -> 
      do { freshFlow <- getFreshFlowVar
         ; return ([genClauseValueDef org freshFlow (TBA.VInt org i)], freshFlow)
         }
    TBN.ExprVar org v -> 
      do { freshFlow <- getFreshFlowVar
         ; cellVar <- cellVarLookup (getVarName v)
         ; return ([genClauseCellGet org freshFlow cellVar], freshFlow)
         }
    x -> error ("could not match " ++ show x)

-- | Generators for clause types for use in aTransform
    
genClauseBinOp :: TBA.Origin -> TBA.FlowVar -> TBA.FlowVar -> TBN.ArithOp-> TBA.FlowVar -> TBA.Clause
genClauseBinOp org flow left op right = TBA.RedexDef org flow (TBA.BinOp org left binop right)
                                         where binop = case op of      
                                                         TBN.Add o -> TBA.OpPlus o
                                                         TBN.Sub o -> TBA.OpMinus o
                                                         TBN.CompEq o -> TBA.OpEqual o
                                                         TBN.Lt o -> TBA.OpLess o  
                                                         TBN.Gt o -> TBA.OpGreater o
                                                         _ -> error "Interpreter does not support >= or <="
getVarName :: TBN.Var -> String
getVarName (TBN.VarDef _ s) = s

genClauseValueDef :: TBA.Origin -> TBA.FlowVar -> TBA.Value -> TBA.Clause
genClauseValueDef org flow value = TBA.Evaluated (TBA.ValueDef org flow value)
        
genClauseOnion :: TBA.Origin -> TBA.FlowVar -> TBA.FlowVar -> TBA.FlowVar -> TBA.Clause
genClauseOnion org fnew f1 f2 = genClauseValueDef org fnew (TBA.VOnion org f1 f2)

genClauseAppl :: TBA.Origin -> TBA.FlowVar -> TBA.FlowVar -> TBA.FlowVar -> TBA.Clause
genClauseAppl org flow f1 f2 = TBA.RedexDef org flow (TBA.Appl org f1 f2)

genClauseCellGet :: TBA.Origin  -> TBA.FlowVar -> TBA.CellVar -> TBA.Clause
genClauseCellGet = TBA.CellGet

genClauseCellSet :: TBA.Origin -> TBA.CellVar -> TBA.FlowVar -> TBA.Clause
genClauseCellSet  = TBA.CellSet

genClauseCellDef :: TBA.Origin -> TBA.CellVar -> TBA.FlowVar -> TBA.Clause
genClauseCellDef org cell flow  = TBA.Evaluated (TBA.CellDef org (TBA.QualNone org) cell flow)

-- | Generators for value types
genLabelValue :: TBN.Label -> TBA.CellVar -> TBA.Value 
genLabelValue (TBN.LabelDef o s) cell =  TBA.VLabel o (TBA.LabelName o s) cell 

-- | Setup state for performTransformation:

testLocation :: SourceRegion
testLocation = SourceRegion Unknown Unknown

testOrigin :: TBA.Origin
testOrigin = TBA.SourceOrigin testLocation

startState :: TState
startState = (0, 0, empty)

performTransformation :: TBN.Expr -> TBA.Expr
performTransformation expr = TBA.Expr testOrigin $ fst $ evalState (aTransform expr) startState