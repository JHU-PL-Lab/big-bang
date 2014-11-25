{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances, GeneralizedNewtypeDeriving  #-}
{-|
  This module defines the type level functions specifying the behavior of
  TinyBang built-in operations.
-}
module Language.TinyBang.TypeSystem.Simple.Closure.Builtins
( builtinTypeEval
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.NonDet
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Contours
import Language.TinyBang.TypeSystem.Simple.Closure.Basis
import Language.TinyBang.TypeSystem.Simple.Compatibility
import Language.TinyBang.TypeSystem.Simple.Data
import Language.TinyBang.TypeSystem.Simple.InitialAlignment
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

-- * Built-in Closure Evaluation

-- TODO: this module should really line up better with the evaluation side of
--       things.  In practice, evaluation should be using strict pattern
--       matching to bind and pick out the arguments to the builtin operator;
--       then, the type system could follow suit.

-- |Defines the behavior of the built-in operator functions in the TinyBang
--  language specification.  The result is a set of constraints to add to the
--  existing constraint set and the type of the result.
builtinTypeEval :: BuiltinOp -> [TVar] -> TVar
                -> ClosureM (ConstraintSet, Type)
builtinTypeEval op as a = do
  (result, cs) <- runBuiltinClosureM op as a computeBuiltinType
  case result of
    Left incon ->
      reportInconsistency incon
    Right t ->
      return (cs, t)
  
newtype BuiltinClosureM a
  = BuiltinClosureM
      { unBuiltinClosureM ::
          EitherT Inconsistency
            (WriterT ConstraintSet
              (ReaderT BuiltinClosureStepContext ClosureM))
          a
      }
  deriving ( Monad, Functor, Applicative
           , MonadNonDet, MonadReader BuiltinClosureStepContext
           , MonadWriter ConstraintSet)

data BuiltinClosureStepContext =
  BuiltinClosureStepContext ConstraintSet BuiltinOp [TVar] TVar

runBuiltinClosureM :: BuiltinOp
                   -> [TVar]
                   -> TVar
                   -> BuiltinClosureM a
                   -> ClosureM (Either Inconsistency a, ConstraintSet)
runBuiltinClosureM bop operands defnSite x = do
  cs <- ask
  let context = BuiltinClosureStepContext cs bop operands defnSite
  let eitherPart = runEitherT $ unBuiltinClosureM x
  let writerPart = runWriterT eitherPart
  let readerPart = runReaderT writerPart context
  readerPart

biCs :: BuiltinClosureM ConstraintSet
biCs = (\(BuiltinClosureStepContext cs _ _ _) -> cs) <$> ask

biOp :: BuiltinClosureM BuiltinOp
biOp = (\(BuiltinClosureStepContext _ bop _ _) -> bop) <$> ask

biArgs :: BuiltinClosureM [TVar]
biArgs = (\(BuiltinClosureStepContext _ _ args _) -> args) <$> ask

biDefnSite :: BuiltinClosureM TVar
biDefnSite = (\(BuiltinClosureStepContext _ _ _ site) -> site) <$> ask

biLift :: ClosureM a -> BuiltinClosureM a
biLift = BuiltinClosureM . lift . lift . lift

raiseInconsistency :: Inconsistency -> BuiltinClosureM a
raiseInconsistency incon = BuiltinClosureM $ left incon

biBadOperandCount :: Int -> Int -> BuiltinClosureM a
biBadOperandCount expected appeared =
  raiseInconsistency =<<
    BuiltinBadOperandCount <$>
      biDefnSite <*>
      biOp <*>
      pure expected <*>
      pure appeared

biBadOperandType :: Int -> TVar -> BuiltinClosureM a
biBadOperandType index a =
  raiseInconsistency =<<
    BuiltinBadOperandType <$>
      biDefnSite <*>
      biOp <*>
      pure index <*>
      pure a

computeBuiltinType :: BuiltinClosureM Type
computeBuiltinType = do
  op <- biOp
  case op of
    OpIntPlus -> intComputeOp
    OpIntMinus  -> intComputeOp
    OpIntEq -> intCompareOp
    OpIntLessEq -> intCompareOp
    OpIntGreaterEq -> intCompareOp
    OpSet -> do
      (a1,a2) <- demand2
      a' <- demandRef 1 a1
      tell $ ConstraintSet $ Set.singleton $ a2 <: a'
      return TEmptyOnion 
  where
    demand2 :: BuiltinClosureM (TVar,TVar)
    demand2 = do
      operands <- biArgs
      case operands of
        [x1,x2] -> return (x1,x2)
        _ -> biBadOperandCount 2 $ length operands
    demandMatch :: PatternType -> Int -> TVar -> BuiltinClosureM ConstraintSet
    demandMatch pat index a = do
      cs <- biCs
      mbindings <- choose $ Set.toList $ findCompatibilityCases a cs pat mempty
      case mbindings of
        Nothing -> biBadOperandType index a
        Just bindings -> return bindings
    demandInt :: Int -> TVar -> BuiltinClosureM ()
    demandInt index a = do
      let pvar = initiallyAlignVar $ builtinVar $
                    PrimitiveMatchPatternVar PrimInt
      let pat = PatternType pvar $ Map.singleton pvar $ TFPrim PrimInt
      void $ demandMatch pat index a
    demandRef :: Int -> TVar -> BuiltinClosureM TVar
    demandRef index a = do
      let pvar1 = initiallyAlignVar $ builtinVar $ RefMatchPatternVar 1
      let pvar2 = initiallyAlignVar $ builtinVar $ RefMatchPatternVar 2
      let pat = PatternType pvar1 $ Map.singleton pvar1 $ TFRef pvar2
      bindings <- demandMatch pat index a
      -- PROBLEM: without strict matching, there's no way to get the specific
      --          variable inside of the ref (since pattern matching
      --          automatically reaches into the ref variable)
      error "Language.TinyBang.TypeSystem.Simple.Closure.Builtins:demandRef undefined" -- TODO
    intComputeOp :: BuiltinClosureM Type
    intComputeOp = do
      (a1,a2) <- demand2
      demandInt 1 a1
      demandInt 2 a2
      return $ TPrimitive PrimInt
    intCompareOp :: BuiltinClosureM Type
    intCompareOp = do
      (a1,a2) <- demand2
      demandInt 1 a1
      demandInt 2 a2
      -- NOTE: The variable a' need not be assigned a new () lower bound; it
      --       will already have one at top level.  But we need to make sure
      --       that a' is properly contoured; in concept, the instantiation of
      --       the initial contour freshened the top-level definition of a' and
      --       we must refer to it using its contour (instead of with no
      --       contour).
      TVar x _ <- initiallyAlignVar <$> builtinVar <$> BuiltinVar <$> biOp
      let a' = TVar x $ PossibleContour $ Just initialContour
      choose $
        map (flip TLabel a' . LabelName generated) ["True", "False"] 

-- TODO: maybe move this to a more general location so evaluation can use it?
builtinVar :: VarName -> Var
builtinVar vn = Var generated vn Nothing
