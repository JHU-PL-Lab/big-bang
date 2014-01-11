{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Language.TinyBang.Interpreter.Builtins
( evalBuiltin
, builtinEnv

, BuiltinSpecifier(..)
, getBuiltinSpecifier
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Maybe

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis

-- * Builtin evaluation

-- |A little monad for builtin evaluation.  Primarily used to avoid passing
--  arguments to helper functions.
newtype BuiltinEvalM a =
  BuiltinEvalM
    { unBuiltinEvalM :: ReaderT BuiltinEvalContext EvalM a
    }
  deriving (Monad, Applicative, Functor, MonadReader BuiltinEvalContext)

newtype BuiltinEvalContext =
  BuiltinEvalContext { unBuiltinEvalContext :: (Origin, (BuiltinOp, [Var])) }
  deriving (Show)

biOrigin :: BuiltinEvalM Origin
biOrigin = fst . unBuiltinEvalContext <$> ask

biOp :: BuiltinEvalM BuiltinOp
biOp = fst . snd . unBuiltinEvalContext <$> ask

biVars :: BuiltinEvalM [Var]
biVars = snd . snd . unBuiltinEvalContext <$> ask

biLift :: EvalM a -> BuiltinEvalM a
biLift = BuiltinEvalM . lift
  
-- |A function which performs the interpretation of a builtin.
evalBuiltin :: Origin -> BuiltinOp -> [Var] -> EvalM Value
evalBuiltin o bop vs =
  let spec = getBuiltinSpecifier bop in
  runReaderT (unBuiltinEvalM (builtinEval spec)) $
    BuiltinEvalContext (o, (bop, vs))

demand2 :: BuiltinEvalM (Var, Var)
demand2 = do
  vs <- biVars
  case vs of
    x1:x2:[] -> return (x1,x2)
    _ -> do
      o <- biOrigin
      bop <- biOp
      biLift $ raiseEvalError $ BuiltinBadOperandCount o bop 2 $ length vs

project :: forall a. Int -> (Value -> Maybe a) -> Var -> BuiltinEvalM a
project idx f x = do
  opts <- catMaybes <$> projOpts x
  case listToMaybe opts of
    Just r -> return r
    Nothing -> do
      o <- biOrigin
      bop <- biOp
      biLift $ raiseEvalError $ BuiltinBadOperandType o bop idx x
  where
    projOpts :: Var -> BuiltinEvalM [Maybe a]
    projOpts x' = do
      v <- biLift $ varLookup x'
      case v of
        VOnion _ x1 x2 ->
          liftM2 (++) (projOpts x1) (projOpts x2)
        _ ->
          return [f v]

expectInt :: Value -> Maybe (Origin, Integer)
expectInt v =
  case v of
    VPrimitive o' (VInt _ n) -> Just (o',n)
    _ -> Nothing
    
evalPlus :: BuiltinEvalM Value
evalPlus = do
  (x1,x2) <- demand2
  (o1,n1) <- project 0 expectInt x1
  (o2,n2) <- project 1 expectInt x2
  return $ VPrimitive (ComputedOrigin [o1,o2]) $ VInt generated $ n1 + n2

-- * Builtin environment

-- |The definition of the builtin environment.
builtinEnv :: [Clause]
builtinEnv = map (builtinOpToClause . getBuiltinSpecifier) $ enumFrom minBound
  where
    builtinOpToClause :: BuiltinSpecifier -> Clause
    builtinOpToClause spec =
      let (patternParts, vars) = unzip $ builtinPatterns spec in
      let subPatterns = zipWith makeComponentPattern ordinalLabelNames
                          patternParts in
      let pattern = makeConjunctions subPatterns in
      Evaluated $ ValueDef generated (Var generated $ varname spec) $
        VScape generated (Pattern generated pattern) $
          Expr generated
            [RedexDef generated (Var generated $ varname spec ++ "__return") $
              Builtin generated (builtinOperator spec) vars]
      where
        ordinalLabelNames :: [LabelName]
        ordinalLabelNames = map (LabelName generated . show) [1::Integer ..]
        endVar :: [PatternClause] -> Var
        endVar pcls = case last pcls of PatternClause _ x _ -> x
        makeComponentPattern :: LabelName -> [PatternClause] -> [PatternClause]
        makeComponentPattern ln pat =
          pat ++
            [PatternClause generated
              (Var generated $ varname spec ++ "__label" ++ unLabelName ln) $
                PLabel generated ln $ endVar pat]
        makeConjunctions :: [[PatternClause]] -> [PatternClause]
        makeConjunctions pclss =
          if null pclss
            then [PatternClause generated
                    (Var generated $ varname spec ++ "__empty") $
                    PEmptyOnion generated]
            else foldl combineGroups (head pclss) (zip (tail pclss) [1::Int ..])
          where
            combineGroups :: [PatternClause]
                          -> ([PatternClause], Int)
                          -> [PatternClause]
            combineGroups pcls1 (pcls2, idx) =
              case (null pcls1, null pcls2) of
                (True, _) -> pcls2
                (False, True) -> pcls1
                (False, False) ->
                  pcls1 ++ pcls2 ++
                    [PatternClause generated
                      (Var generated $ varname spec ++ "__onion" ++ show idx) $
                      PConjunction generated (endVar pcls1) (endVar pcls2)]

-- * Builtin specification

-- |A data type containing the specification of a builtin.  This includes the
--  variable name of the builtin function, patterns that recognize each operand,
--  the builtin operator to be used, and the Haskell evaluation function that
--  performs the computation.
data BuiltinSpecifier
  = BuiltinSpecifier
      { varname :: String
      , builtinPatterns :: [([PatternClause], Var)]
      , builtinOperator :: BuiltinOp
      , builtinEval :: BuiltinEvalM Value
      }

-- |A function which obtains the specifier for a given builtin.
getBuiltinSpecifier :: BuiltinOp -> BuiltinSpecifier
getBuiltinSpecifier bop =
  case bop of
    Plus -> plusSpecifier
  
-- Each specifier is written separately as a top-level value to avoid
-- recomputation
  
-- |The builtin specifier for plus.
plusSpecifier :: BuiltinSpecifier
plusSpecifier =
  let name = "__plus" in
  BuiltinSpecifier
    { varname = name
    , builtinPatterns =
        [ generateIntPattern name 1
        , generateIntPattern name 2
        ]
    , builtinOperator = Plus
    , builtinEval = evalPlus
    }

-- |Generates a pattern clause for an int.  Uses the provided variable name
--  prefix.
generateIntPattern :: String -> Int -> ([PatternClause], Var)
generateIntPattern name idx =
  ( [ PatternClause generated (variable 1) $ PPrimitive generated PrimInt
    , PatternClause generated (variable 2) $ PEmptyOnion generated
    , PatternClause generated (variable 3) $
        PConjunction generated (variable 1) (variable 2)
    ]
  , variable 2
  )
  where
    variable :: Int -> Var
    variable n = Var generated $ name ++ "__operand" ++ show idx ++ "__pos" ++
                    show n
