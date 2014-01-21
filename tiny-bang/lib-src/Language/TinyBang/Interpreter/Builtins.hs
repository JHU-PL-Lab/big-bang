{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Language.TinyBang.Interpreter.Builtins
( evalBuiltin
, builtinEnv

, BuiltinSpecifier(..)
, getBuiltinSpecifier
, getBuiltinReturnVar
, getBuiltinEmptyOnionVar
, getBuiltinPattern
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Maybe

import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.Basis

-- TODO: consider moving this module to AST and then having the interpreter
--       code simply translate the AST module error to an Interpreter.Basis
--       error

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

expectRef :: Value -> Maybe (Origin, Var)
expectRef v =
  case v of
    VRef o' x -> Just (o', x)
    _ -> Nothing

acceptAnything :: Value -> Maybe (Origin, Value)
acceptAnything v = Just (originOf v, v)

evalIntOp :: (Integer -> Integer -> Integer) -> BuiltinEvalM Value
evalIntOp f = do
  (x1,x2) <- demand2
  (o1,n1) <- project 0 expectInt x1
  (o2,n2) <- project 1 expectInt x2
  return $ VPrimitive (ComputedOrigin [o1,o2]) $ VInt generated $ f n1 n2
    
evalIntPlus :: BuiltinEvalM Value
evalIntPlus = evalIntOp (+)

evalIntMinus :: BuiltinEvalM Value
evalIntMinus = evalIntOp (-)

evalIntCompare :: BuiltinSpecifier
               -> (Integer -> Integer -> Bool)
               -> BuiltinEvalM Value
evalIntCompare spec f = do
  (x1,x2) <- demand2
  (o1,n1) <- project 0 expectInt x1
  (o2,n2) <- project 1 expectInt x2
  let x' = getBuiltinEmptyOnionVar spec
  biLift $ setVar x' $ VEmptyOnion generated
  let lname = LabelName generated $ if f n1 n2 then "True" else "False"
  return $ VLabel (ComputedOrigin [o1,o2]) lname x'

evalIntEq :: BuiltinEvalM Value
evalIntEq = evalIntCompare intLessEqSpecifier (==)

evalIntLessEq :: BuiltinEvalM Value
evalIntLessEq = evalIntCompare intLessEqSpecifier (<=)

evalIntGreaterEq :: BuiltinEvalM Value
evalIntGreaterEq = evalIntCompare intGreaterEqSpecifier (>=)

evalSet :: BuiltinEvalM Value
evalSet = do
  (x1,x2) <- demand2
  -- TODO: use origins from projection to annotate something here
  (_,cellX) <- project 0 expectRef x1
  (_,newValue) <- project 1 acceptAnything x2
  biLift $ setVar cellX newValue
  return $ VEmptyOnion generated

-- * Builtin environment

-- |The definition of the builtin environment.
builtinEnv :: [Clause]
builtinEnv = map (builtinOpToClause . getBuiltinSpecifier) $ enumFrom minBound
  where
    builtinOpToClause :: BuiltinSpecifier -> Clause
    builtinOpToClause spec =
      let vars = snd $ unzip $ builtinPatterns spec in
      let pattern = getBuiltinPattern spec in
      Evaluated $ ValueDef generated (Var generated $ varname spec) $
        VScape generated (Pattern generated pattern) $
          Expr generated
            [RedexDef generated (getBuiltinReturnVar spec) $
              Builtin generated (builtinOperator spec) vars]

endVar :: [PatternClause] -> Var
endVar pcls = case last pcls of PatternClause _ x _ -> x

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
    OpIntPlus -> intPlusSpecifier
    OpIntMinus -> intMinusSpecifier
    OpIntEq -> intEqSpecifier
    OpIntLessEq -> intLessEqSpecifier
    OpIntGreaterEq -> intGreaterEqSpecifier
    OpSet -> setSpecifier

getBuiltinVar :: BuiltinSpecifier -> String -> Var
getBuiltinVar spec name = Var generated $ varname spec ++ "__" ++ name

-- |Creates a variable for a builtin pattern.  The first @Int@ is the operand
--  index; the second @Int@ is the variable index at that operand.
getBuiltinPatternVar :: BuiltinSpecifier -> Int -> Int -> Var
getBuiltinPatternVar spec opn varn =
  getBuiltinVar spec $ "operand" ++ show opn ++ "__pos" ++ show varn 

-- |A function which retrieves the output variable for a given builtin
--  specifier.
getBuiltinReturnVar :: BuiltinSpecifier -> Var
getBuiltinReturnVar spec = getBuiltinVar spec "return"

-- |A function which retrieves the output variable for a given builtin
--  specifier.
getBuiltinEmptyOnionVar :: BuiltinSpecifier -> Var
getBuiltinEmptyOnionVar spec = getBuiltinVar spec "emptyonion"

-- |A function to obtain the pattern for a given built-in function.
getBuiltinPattern :: BuiltinSpecifier -> [PatternClause]
getBuiltinPattern spec =
  let patternParts = fst $ unzip $ builtinPatterns spec in
  let subPatterns = zipWith makeComponentPattern ordinalLabelNames
                      patternParts in
  let pattern = makeConjunctions subPatterns in
  pattern
    where
      ordinalLabelNames :: [LabelName]
      ordinalLabelNames = map (LabelName generated . show) [1::Integer ..]
      makeComponentPattern :: LabelName -> [PatternClause] -> [PatternClause]
      makeComponentPattern ln pat =
        pat ++
          [PatternClause generated
            (getBuiltinVar spec $ "label" ++ unLabelName ln) $
              PLabel generated ln $ endVar pat]
      makeConjunctions :: [[PatternClause]] -> [PatternClause]
      makeConjunctions pclss =
        if null pclss
          then [PatternClause generated
                  (getBuiltinVar spec "empty") $ PEmptyOnion generated]
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
                    (getBuiltinVar spec $ "onion" ++ show idx) $
                    PConjunction generated (endVar pcls1) (endVar pcls2)]

-- Each specifier is written separately as a top-level value to avoid
-- recomputation
  
-- |The builtin specifier for integer plus.
intPlusSpecifier :: BuiltinSpecifier
intPlusSpecifier =
  BuiltinSpecifier
    { varname = "__intplus"
    , builtinPatterns =
        [ generateIntPattern intPlusSpecifier 1
        , generateIntPattern intPlusSpecifier 2
        ]
    , builtinOperator = OpIntPlus
    , builtinEval = evalIntPlus
    }

-- |The builtin specifier for integer minus.
intMinusSpecifier :: BuiltinSpecifier
intMinusSpecifier =
  BuiltinSpecifier
    { varname = "__intminus"
    , builtinPatterns =
        [ generateIntPattern intMinusSpecifier 1
        , generateIntPattern intMinusSpecifier 2
        ]
    , builtinOperator = OpIntMinus
    , builtinEval = evalIntMinus
    }

-- |The builtin specifier for integer equal-to.
intEqSpecifier :: BuiltinSpecifier
intEqSpecifier =
  BuiltinSpecifier
    { varname = "__inteq"
    , builtinPatterns =
        [ generateIntPattern intEqSpecifier 1
        , generateIntPattern intEqSpecifier 2
        ]
    , builtinOperator = OpIntEq
    , builtinEval = evalIntEq
    }

-- |The builtin specifier for integer less-than-or-equal-to.
intLessEqSpecifier :: BuiltinSpecifier
intLessEqSpecifier =
  BuiltinSpecifier
    { varname = "__intleq"
    , builtinPatterns =
        [ generateIntPattern intLessEqSpecifier 1
        , generateIntPattern intLessEqSpecifier 2
        ]
    , builtinOperator = OpIntLessEq
    , builtinEval = evalIntLessEq
    }

-- |The builtin specifier for integer greater-than-or-equal-to.
intGreaterEqSpecifier :: BuiltinSpecifier
intGreaterEqSpecifier =
  BuiltinSpecifier
    { varname = "__intgeq"
    , builtinPatterns =
        [ generateIntPattern intGreaterEqSpecifier 1
        , generateIntPattern intGreaterEqSpecifier 2
        ]
    , builtinOperator = OpIntGreaterEq
    , builtinEval = evalIntGreaterEq
    }

-- |The builtin specifier for cell assignment.
setSpecifier :: BuiltinSpecifier
setSpecifier =
  BuiltinSpecifier
    { varname = "__set"
    , builtinPatterns =
        [ ( [ PatternClause generated (variable 1 1) $
                PEmptyOnion generated
            , PatternClause generated (variable 1 2) $
                PRef generated $ variable 1 1
            , PatternClause generated (variable 1 3) $
                PEmptyOnion generated
            , PatternClause generated (variable 1 4) $
                PConjunction generated (variable 1 2) (variable 1 3)
            ]
          , variable 1 4
          )
        , ( [ PatternClause generated (variable 2 1) $
                PEmptyOnion generated
            ]
          , variable 2 1
          )
        ]
    , builtinOperator = OpSet
    , builtinEval = evalSet
    }
  where
    variable = getBuiltinPatternVar setSpecifier
    
-- |Generates a pattern clause for an int.  Uses the provided variable name
--  prefix.
generateIntPattern :: BuiltinSpecifier -> Int -> ([PatternClause], Var)
generateIntPattern spec idx =
  ( [ PatternClause generated (variable 1) $ PPrimitive generated PrimInt
    , PatternClause generated (variable 2) $ PEmptyOnion generated
    , PatternClause generated (variable 3) $
        PConjunction generated (variable 1) (variable 2)
    ]
  , variable 2
  )
  where
    variable :: Int -> Var
    variable = getBuiltinPatternVar spec idx
