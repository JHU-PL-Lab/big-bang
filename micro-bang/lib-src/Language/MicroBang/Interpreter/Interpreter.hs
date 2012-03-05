module Language.MicroBang.Interpreter.Interpreter
  ( evalTop
  , EvalError(..)
  , EvalSuccessOrFailure(..)
  , EvalStringResult(..)
  , EvalStringM(..)
  , EvalStringResultErrorWrapper(..)) where

import Control.Monad.RWS (RWS, evalRWS)
import Control.Monad.Error (Error, ErrorT, strMsg, throwError, runErrorT)
import Control.Monad.State (StateT, runStateT, get, put, gets, modify)
import Control.Monad.Reader (ReaderT, Reader, asks, ask, runReader)
import Control.Monad.Identity (Identity)

import Control.Monad.Writer (tell, listen, execWriter, Writer)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Language.MicroBang.Syntax.Lexer as L
import qualified Language.MicroBang.Syntax.Parser as P


import Language.MicroBang.Ast
  ( Expr(..)
  , Operator(..)
  , Chi(..)
  , Branches
  , Branch(..)
  , Value(..)
  , exprFromValue
  , Evaluated(..)
  )
import Language.MicroBang.Types.UtilTypes
    ( Ident
    , unIdent
    , LabelName
    , labelName
    )
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
  deriving (Show)

data SX = XAny | XTInt | XTUnit | XLabel LabelName ProgramPoint | XFun
  deriving (Show)
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
  deriving (Show)
data FBinOp = FPlus | FEquals
  deriving (Show)
data FOnionSub = FSubInt | FSubUnit | FSubLabel LabelName | FSubFun
  deriving (Show)
-- Constraints
-- |A type alias defining the form of constraints.
type Constraints = Set Constraint
-- |A type describing constraints in Little Bang.
data Constraint
  = SLower SDown ProgramPoint
  | SIntermediate ProgramPoint ProgramPoint
  | SUpper ProgramPoint SUp
  | SOp ProgramPoint FBinOp ProgramPoint ProgramPoint
  | SCase ProgramPoint [(SX, [Constraints])]
  deriving (Show)


-- Errors

-- I.EvalError, From TinyBang Interpreter.
data EvalError =
      NoConcreteValue
    | NonFunctionApplication
    | CaseContradiction
    | IntegerOperationFailure
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
      EvalResult e esof -> text "Got a result!"
      Contradiction e cs-> text "Contradiction"
      DerivationFailure e cs -> text "DerivationFailure"
      ParseFailure pe -> text "ParseFailure"
      LexFailure s -> text "LexFailure" <+> text s


-- |A monadic exception type for the evalStringTop process.
data EvalStringResultErrorWrapper =
    FailureWrapper EvalStringResult
instance Error EvalStringResultErrorWrapper where
    strMsg = error



type PId = Integer
type M = Map Ident ProgramPoint
type NextFreshVar = PId

type CEM a = ErrorT ConstraintEvaluatorError
                    (RWS M Constraints NextFreshVar)
                    a



-- Start here. :)
evalTop :: Expr -> Either EvalError Value
evalTop e =
  let cemProgPt = derive e in
  --let (progPt, cs) = runCEM (Map.empty) 0 in
  --let cs' = close cs in
  --lookup progPt cs'
  Right VEmptyOnion


derive :: Expr -> CEM ProgramPoint
derive (Var i) = (error (show i)) -- M|-x:p1\Null, where x:p1 E M
derive (Label labelName e) = let p2 = (derive e) in error (show labelName) -- p1 \ {lbl p2<:p1}U F, where e:p2\F
derive (Onion e1 e2) = error ((show e1) ++ "&&" ++ (show e2))
derive (OnionSub e sub) = error ((show e) ++ "&&" ++ (show sub))
derive EmptyOnion = error "EmptyOnion"
derive (Func ident e) = error "Function"
derive (Appl e1 e2) = error "Appl"
derive (PrimInt i) = error ("PrimInt " ++ (show i))
derive PrimUnit = error "PrimUnit"
derive (Case e bs) = error "Case"
derive (BinOp op e1 e2) = error "BinOp"

--runCEM :: (Map.Map k a) -> Int -> (ProgramPoint, Constraints)
--close :: Constraints -> Constraints
--retrieve :: ProgramPoint -> Constraints -> Value


