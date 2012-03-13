module Language.MicroBang.Interpreter.Interpreter
  ( evalTop
  , EvalError(..)
  , EvalSuccessOrFailure(..)
  , EvalStringResult(..)
  , EvalStringM(..)
  , EvalStringResultErrorWrapper(..)) where

import Control.Monad.RWS (RWS, evalRWS, censor)
import Control.Monad.Error (Error, ErrorT, strMsg, throwError, runErrorT)
import Control.Monad.State (StateT, runStateT, get, put, gets, modify)
import Control.Monad.Reader (ReaderT, Reader, asks, ask, runReader, local)
import Control.Monad.Identity (Identity)
import Control.Monad.Writer (tell, listen, execWriter, Writer)

import Data.Monoid (Monoid, mempty)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, fold)
import qualified Data.Set as Set

import qualified Language.MicroBang.Syntax.Lexer as L
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
import Language.MicroBang.Types.UtilTypes as UT
import Utils.Render.Display


data ConstraintEvaluatorError
      -- |Indicates that an expression contained an unbound variable.
      = NotClosed Ident
      -- |Indicates that the binder and label use the same variable.
      | DoubleBound Ident
      -- TODO: Add NotImplemented
    deriving (Show)
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
data ProgramPoint = ProgramPoint Integer
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
  | SDFunction ProgramPoint ProgramPoint Constraints
  | SDBadness
  deriving (Show, Eq, Ord)
data FOnionSub = FSubInt | FSubUnit | FSubLabel LabelName | FSubFun
  deriving (Show, Eq, Ord)

convertSubTermToFOnionSub :: UT.SubTerm -> FOnionSub
convertSubTermToFOnionSub (UT.SubPrim (UT.PrimInt)) = FSubInt
convertSubTermToFOnionSub (UT.SubPrim (UT.PrimUnit)) = FSubUnit
convertSubTermToFOnionSub (UT.SubLabel ln) = FSubLabel ln
convertSubTermToFOnionSub UT.SubFunc = FSubFun

-- Constraints
-- |A type alias defining the form of constraints.
type Constraints = Set Constraint
-- |A type describing constraints in Little Bang.
data Constraint
  = SLower SDown ProgramPoint
  | SIntermediate ProgramPoint ProgramPoint
  | SUpper ProgramPoint SUp
  | SOp ProgramPoint Operator ProgramPoint ProgramPoint
  | SCase ProgramPoint [(SX, [Constraints])]
  deriving (Show, Ord, Eq)


-- Errors

-- I.EvalError, From TinyBang Interpreter.
data EvalError =
      NoConcreteValue
    | NonFunctionApplication
    | CaseContradiction
    | IntegerOperationFailure
    | ConstraintFailure
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
      Contradiction e cs-> text "Contradiction"
      DerivationFailure e cs -> text "DerivationFailure"
      ParseFailure pe -> text "ParseFailure"
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
    return $ ProgramPoint idx



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
      Left f -> Left ConstraintFailure
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
derive (Label labelName e) = do
  -- p1 \ {lbl p2<:p1}U F, where e:p2\F
  p2 <- (derive e)
  p1 <- freshVar
  tell (Set.singleton (SLower (SDLabel labelName p2) p1))
  return p1
derive (BinOp op e1 e2) = do
  p1 <- derive e1
  p2 <- derive e2
  p0 <- freshVar
  tell (Set.singleton (SOp p1 op p2 p0))
  return p0
derive (Appl e1 e2) = error "Appl"
derive (Case e bs) = error "Case"

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
  tell (Set.singleton (SLower (SDFunction p2 p3 f) p1))
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


runCEM :: CEM a -> M -> NextFreshVar -> (Either ConstraintEvaluatorError a, Constraints)
runCEM c r s = evalRWS (runErrorT c) r s

close :: Constraints -> Constraints
close cs = error $ show cs

retrieve :: ProgramPoint -> Constraints -> Value
retrieve p cs =
  case fold (find p) Nothing cs of
    Nothing -> error "Could not retrieve"
    Just sd -> case sd of
      SDUnit -> VPrimUnit
      SDInt i -> VPrimInt i
      SDLabel ln p1 -> VLabel ln (retrieve p1 cs)
      SDOnion p1 p2 -> VOnion (retrieve p1 cs) (retrieve p2 cs)
      SDOnionSub p1 s1 -> error "Subtration Onion?"
      SDEmptyOnion -> VEmptyOnion
      SDFunction p1 p2 cs -> error "Function...what do I do..."
      SDBadness -> error "Lightening"

  where
    find :: ProgramPoint -> Constraint -> Maybe SDown -> Maybe SDown
    find p1 (SLower sd p2) Nothing | (p1 == p2) = Just sd
    find _ _ Nothing = Nothing
    find _ _ mc = mc
