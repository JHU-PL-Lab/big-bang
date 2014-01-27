{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances, GeneralizedNewtypeDeriving  #-}
{-|
  This module defines the type level functions specifying the behavior of
  TinyBang built-in operations.
-}
module Language.TinyBang.TypeSystem.Closure.Builtins
( builtinTypeEval
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Either

import Language.TinyBang.Ast
import Language.TinyBang.TypeSystem.Closure.Basis
import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.ConstraintHistory
import Language.TinyBang.TypeSystem.Constraints
import Language.TinyBang.TypeSystem.InitialDerivation
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.NonDet
import Language.TinyBang.TypeSystem.Types
import Language.TinyBang.Utils.Logger
import Language.TinyBang.Utils.Projection

$(loggingFunctions)

-- * Built-in Closure Evaluation

-- |Defines the behavior of the built-in operator functions in the TinyBang
--  language specification.  The result is a set of constraints to add to the
--  existing constraint set and @Maybe@ a type.  If the type is @Nothing@, this
--  indicates an inconsistency occurred; the appropriate inconsistency
--  constraint will appear in the returned set.
builtinTypeEval :: (ConstraintDatabase db)
                => ConstraintHistory db -> BuiltinOp -> [TVar] -> TVar
                -> ClosureStepM db (db, Maybe (Type db))
builtinTypeEval h op as a = do
  (result, db) <- runBuiltinClosureStepM h op as a computeBuiltinType
  return $ case result of
    Left incon ->
      (CDb.union db $ CDb.singleton $ InconsistencyConstraint h incon, Nothing)
    Right t ->
      (db, Just t)
  
newtype BuiltinClosureStepM db a
  = BuiltinClosureStepM
      { unBuiltinClosureStepM ::
          EitherT (Inconsistency db)
            (WriterT db
              (ReaderT (BuiltinClosureStepContext db) (ClosureStepM db)))
          a
      }
  deriving ( Monad, Functor, Applicative, MonadCReader db, MonadNonDet
           , MonadReader (BuiltinClosureStepContext db)
           , MonadWriter db)

data BuiltinClosureStepContext db
  = BuiltinClosureStepContext (ConstraintHistory db) BuiltinOp [TVar] TVar

runBuiltinClosureStepM :: (ConstraintDatabase db)
                       => ConstraintHistory db
                       -> BuiltinOp
                       -> [TVar]
                       -> TVar
                       -> BuiltinClosureStepM db a
                       -> ClosureStepM db (Either (Inconsistency db) a, db)
runBuiltinClosureStepM h bop operands defnSite x =
  let context = BuiltinClosureStepContext h bop operands defnSite in
  let eitherPart = runEitherT $ unBuiltinClosureStepM x in
  let writerPart = runWriterT eitherPart in
  let readerPart = runReaderT writerPart context in
  readerPart

biHist :: (ConstraintDatabase db)
       => BuiltinClosureStepM db (ConstraintHistory db)
biHist = (\(BuiltinClosureStepContext h _ _ _) -> h) <$> ask

biOp :: (ConstraintDatabase db)
     => BuiltinClosureStepM db BuiltinOp
biOp = (\(BuiltinClosureStepContext _ bop _ _) -> bop) <$> ask

biArgs :: (ConstraintDatabase db)
       => BuiltinClosureStepM db [TVar]
biArgs = (\(BuiltinClosureStepContext _ _ args _) -> args) <$> ask

biDefnSite :: (ConstraintDatabase db)
           => BuiltinClosureStepM db TVar
biDefnSite = (\(BuiltinClosureStepContext _ _ _ site) -> site) <$> ask

biLift :: (ConstraintDatabase db)
       => ClosureStepM db a -> BuiltinClosureStepM db a
biLift = BuiltinClosureStepM . lift . lift . lift

raiseInconsistency :: (ConstraintDatabase db)
                   => Inconsistency db -> BuiltinClosureStepM db a
raiseInconsistency incon =
  BuiltinClosureStepM $ left incon

biBadOperandCount :: (ConstraintDatabase db)
                  => Int -> Int -> BuiltinClosureStepM db a
biBadOperandCount expected appeared =
  raiseInconsistency =<<
    BuiltinBadOperandCount <$>
      biDefnSite <*>
      biOp <*>
      pure expected <*>
      pure appeared

biBadOperandType :: (ConstraintDatabase db)
                 => Int -> TVar -> BuiltinClosureStepM db a
biBadOperandType index a =
  raiseInconsistency =<<
    BuiltinBadOperandType <$>
      biDefnSite <*>
      biOp <*>
      pure index <*>
      pure a

computeBuiltinType :: forall db. (ConstraintDatabase db)
                   => BuiltinClosureStepM db (Type db)
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
      h <- biHist
      tell $ CDb.singleton $ a2 <: a' .: h
      return TEmptyOnion 
  where
    demand2 :: BuiltinClosureStepM db (TVar,TVar)
    demand2 = do
      operands <- biArgs
      case operands of
        [x1,x2] -> return (x1,x2)
        _ -> biBadOperandCount 2 $ length operands
    demandProjection :: Projector
                     -> (Type db -> Maybe a)
                     -> Int
                     -> TVar
                     -> BuiltinClosureStepM db a
    demandProjection proj extract index a = do
      mv <- embed $ project proj $ mktov a
      case extract =<< mv of
        Just a' -> return a'
        Nothing -> biBadOperandType index a
    demandInt :: Int -> TVar -> BuiltinClosureStepM db ()
    demandInt = demandProjection (ProjPrim PrimInt) $
                  \t -> case t of
                          TPrimitive PrimInt -> Just ()
                          _ -> Nothing
    demandRef :: Int -> TVar -> BuiltinClosureStepM db TVar
    demandRef = demandProjection ProjRef $
                  \t -> case t of
                          TRef a -> Just a
                          _ -> Nothing
    intComputeOp :: BuiltinClosureStepM db (Type db)
    intComputeOp = do
      (a1,a2) <- demand2
      demandInt 1 a1
      demandInt 2 a2
      return $ TPrimitive PrimInt
    intCompareOp :: BuiltinClosureStepM db (Type db)
    intCompareOp = do
      (a1,a2) <- demand2
      demandInt 1 a1
      demandInt 2 a2
      a' <- derivVar <$> (builtinVar <$> biOp)
      choose $
        map (flip TLabel (mktov a') . LabelName generated) ["True", "False"] 

-- * Projection

newtype ProjectionM db a
  = ProjectionM
      { unProjectionM :: NonDetT (CReader db) a
      }
  deriving ( Monad, Functor, Applicative, MonadCReader db, MonadNonDet
           , MonadPlus)

embed :: (ConstraintDatabase db) => ProjectionM db a -> BuiltinClosureStepM db a
embed x =
  biLift $ ClosureStepM $ unProjectionM x

-- |Defines type-level projection for built-ins.
project :: (ConstraintDatabase db)
        => Projector -> TypeOrVar db -> ProjectionM db (Maybe (Type db)) 
project proj tov = do
  -- Select a lower bound of the variable
  t <- case unTypeOrVar tov of
          Left t ->
            return t
          Right a ->
            join $ choose <$> queryDb (QueryLowerBoundingTypesOfTVar a)
  -- Proceed based on the structure of that type
  let success = return $ Just t
  let failure = return Nothing
  -- FIXME: what about non-contractive types?  Need an occurrence check!
  case t of
    TOnion tov1 tov2 ->
      maybe <$> project proj tov2 <*> pure Just <*> project proj tov1
    TPrimitive pt | proj == ProjPrim pt ->
      success
    TPrimitive _ ->
      failure
    TLabel n _ | proj == ProjLabel n ->
      success
    TLabel _ _ ->
      failure
    TRef _ | proj == ProjRef ->
      success
    TRef _ ->
      failure
    TEmptyOnion ->
      failure
    TScape{} ->
      failure
