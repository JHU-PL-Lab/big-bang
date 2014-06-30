{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeSynonymInstances, GeneralizedNewtypeDeriving  #-}
{-|
  This module defines the basic components of the TinyBang constraint closure
  operation.
-}
module Language.TinyBang.TypeSystem.Closure.Basis
( ClosureStepM(..)
, runClosureStepM
) where

import Control.Applicative
import Control.Monad

import Language.TinyBang.TypeSystem.ConstraintDatabase as CDb
import Language.TinyBang.TypeSystem.Monad.Trans.CReader
import Language.TinyBang.TypeSystem.Monad.Trans.NonDet
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Logger

$(loggingFunctions)

newtype ClosureStepM db a
  = ClosureStepM
      { unClosureStepM :: NonDetT (CReader db) a
      }
  deriving ( Monad, Functor, Applicative, MonadCReader db, MonadNonDet
           , MonadPlus)

runClosureStepM :: (ConstraintDatabase db, Display db)
                => ClosureStepM db a -> db -> [a]
runClosureStepM x db = runCReader (runNonDetT (unClosureStepM x)) db
