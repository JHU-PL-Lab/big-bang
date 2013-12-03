module Language.LittleBang.Translator
( desugarLittleBang,
) where

import qualified Language.LittleBang.Ast as LB
import qualified Language.TinyBang.Ast as TB
import qualified Language.TinyBangNested.Ast as TBN
import Control.Applicative
import Control.Monad.State

-- | Desugar LittleBang. Do nothing for now
desugarLittleBang :: LB.Expr -> Either DesugarError LB.Expr
desugarLittleBang expr =
  runDesugarM $ foldl (>>=) (return expr) desugars
  where
    desugars :: [LB.Expr -> DesugarM LB.Expr]
    desugars =
      [ desugarIfThenElse
      ]

runDesugarM :: DesugarM a -> Either DesugarError a
runDesugarM x = fst <$> runStateT x (DesugarState 0)

type DesugarM = StateT DesugarState (Either DesugarError)

type DesugarError = String

data DesugarState
  = DesugarState { freshVarIdx :: Int }

desugarIfThenElse :: LB.Expr -> DesugarM LB.Expr
desugarIfThenElse = undefined -- TODO 

nextFreshVar :: DesugarM LB.Var
nextFreshVar = do
  s <- get
  let varName = "v" ++ show (freshVarIdx s)
  put $ s { freshVarIdx = freshVarIdx s + 1 }
  return $ LB.Var (TB.ComputedOrigin []) varName
