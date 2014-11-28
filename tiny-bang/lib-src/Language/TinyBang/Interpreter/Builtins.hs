{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Language.TinyBang.Interpreter.Builtins
( evalBuiltin
, builtinEnv
) where

import Control.Applicative
import Control.Monad.Reader

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis
import Language.TinyBang.Utils.Projection

-- * Builtin evaluation

-- |Evaluates a built-in operation.  The origin of the operation is used for
--  tracking purposes.
evalBuiltin :: Origin -> BuiltinOp -> [Var] -> EvalM Value
evalBuiltin o bop xs =
  runBuiltinEvalM o bop xs computeBuiltin
  
-- |The built-in environment prepended to evaluated programs.  This environment
--  defines a number of variables used by the built-in operations which are
--  assumed to be globally unique.
builtinEnv :: [Clause]
builtinEnv =
  map defineEmptyOnionVar
    [ OpIntPlus
    , OpIntMinus
    , OpIntMult
    , OpIntDiv
    , OpIntMod
    , OpIntEq
    , OpIntLessEq
    , OpIntGreaterEq
    ]
  where
    defineEmptyOnionVar :: BuiltinOp -> Clause
    defineEmptyOnionVar op =
      Clause generated (builtinVar op) $ Def generated $ VEmptyOnion generated

newtype BuiltinEvalM a
  = BuiltinEvalM
      { unBuiltinEvalM :: ReaderT BuiltinEvalContext EvalM a
      }
  deriving (Monad, Functor, Applicative, MonadReader BuiltinEvalContext)

data BuiltinEvalContext
  = BuiltinEvalContext Origin BuiltinOp [Var]

runBuiltinEvalM :: Origin -> BuiltinOp -> [Var] -> BuiltinEvalM a -> EvalM a
runBuiltinEvalM o bop operands x =
  runReaderT (unBuiltinEvalM x) (BuiltinEvalContext o bop operands)

biOrigin :: BuiltinEvalM Origin
biOrigin = (\(BuiltinEvalContext o _ _) -> o) <$> ask
  
biOp :: BuiltinEvalM BuiltinOp
biOp = (\(BuiltinEvalContext _ bop _) -> bop) <$> ask

biArgs :: BuiltinEvalM [Var]
biArgs = (\(BuiltinEvalContext _ _ args) -> args) <$> ask

biLift :: EvalM a -> BuiltinEvalM a
biLift = BuiltinEvalM . lift

biBadOperandCount :: Int -> Int -> BuiltinEvalM a
biBadOperandCount expected appeared = do
  err <- BuiltinBadOperandCount <$> biOrigin <*> biOp <*> pure expected <*>
          pure appeared
  biLift $ raiseEvalError err

biBadOperandType :: Int -> Var -> BuiltinEvalM a
biBadOperandType index x = do
  err <- BuiltinBadOperandType <$> biOrigin <*> biOp <*> pure index <*> pure x
  biLift $ raiseEvalError err

computeBuiltin :: BuiltinEvalM Value
computeBuiltin = do
  bop <- biOp
  case bop of
    OpIntPlus -> intComputeOp (+)
    OpIntMinus -> intComputeOp (-)
    OpIntMult -> intComputeOp (*)
    OpIntDiv -> intComputeOp div
    OpIntMod -> intComputeOp mod
    OpIntEq -> intCompareOp (==)
    OpIntLessEq -> intCompareOp (<=)
    OpIntGreaterEq -> intCompareOp (>=)
    OpSet -> do
      (x1,x2) <- demand2
      x' <- demandRef 1 x1
      v <- biLift $ varLookup x2
      biLift $ setVar x' v
      return $ VEmptyOnion generated
    OpGetChar -> do
      demand0
      o <- biOrigin
      biLift $ inputTBChar o
    OpPutChar -> do
      x <- demand1
      c <- demandChar 1 x
      biLift $ outputTBChar c
      return $ VEmptyOnion generated
  where
    demand0 :: BuiltinEvalM ()
    demand0 = do
      operands <- biArgs
      case operands of
        [] -> return ()
        _ -> biBadOperandCount 0 $ length operands
    demand1 :: BuiltinEvalM Var
    demand1 = do
      operands <- biArgs
      case operands of
        [x] -> return x
        _ -> biBadOperandCount 1 $ length operands
    demand2 :: BuiltinEvalM (Var,Var)
    demand2 = do
      operands <- biArgs
      case operands of
        [x1,x2] -> return (x1,x2)
        _ -> biBadOperandCount 2 $ length operands
    demandProjection :: Projector
                     -> (Value -> Maybe a)
                     -> Int
                     -> Var
                     -> BuiltinEvalM a
    demandProjection proj extract index x = do
      mv <- project proj x
      case extract =<< mv of
        Just a -> return a
        Nothing -> biBadOperandType index x
    demandInt :: Int -> Var -> BuiltinEvalM Integer
    demandInt = demandProjection (ProjPrim PrimInt) $
                  \v -> case v of
                          VPrimitive _ (VInt _ n) -> Just n
                          _ -> Nothing
    demandChar :: Int -> Var -> BuiltinEvalM Char
    demandChar = demandProjection (ProjPrim PrimChar) $
                  \v -> case v of
                          VPrimitive _ (VChar _ c) -> Just c
                          _ -> Nothing
    demandRef :: Int -> Var -> BuiltinEvalM Var
    demandRef = demandProjection ProjRef $
                  \v -> case v of
                          VRef _ x -> Just x
                          _ -> Nothing
    intComputeOp :: (Integer -> Integer -> Integer) -> BuiltinEvalM Value
    intComputeOp f = do
      (x1,x2) <- demand2
      VPrimitive generated <$> VInt generated <$>
        (f <$> demandInt 1 x1 <*> demandInt 2 x2)
    intCompareOp :: (Integer -> Integer -> Bool)
                 -> BuiltinEvalM Value
    intCompareOp f = do
      (x1,x2) <- demand2
      -- NOTE: The following is assumed to have been bound in scope by the
      --       interpreter, which prefixes builtinEnv before evaluating.
      x' <- builtinVar <$> biOp
      result <- f <$> demandInt 1 x1 <*> demandInt 2 x2
      let name = if result then "True" else "False"
      return $ VLabel generated (LabelName generated name) x'

-- * Projection

project :: Projector -> Var -> BuiltinEvalM (Maybe Value)
project proj x = do
  v <- biLift $ varLookup x
  let requireProj proj' =
        return $ if proj == proj' then Just v else Nothing
  case v of
    VOnion _ x1 x2 ->
      maybe <$> project proj x2 <*> return Just <*> project proj x1
    VPrimitive _ (VInt _  _) ->
      requireProj $ ProjPrim PrimInt
    VPrimitive _ (VChar _  _) ->
      requireProj $ ProjPrim PrimChar
    VLabel _ n _ ->
      requireProj $ ProjLabel n
    VRef _ _ ->
      requireProj ProjRef
    VEmptyOnion _ ->
      failure
    VScape _ _ _ ->
      failure
  where
    failure = return Nothing
