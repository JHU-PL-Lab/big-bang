{-# LANGUAGE FlexibleInstances, GADTs #-}

{-|
  This module defines constraint databases.  It also exports known
  implementations thereof.
-}
module Language.TinyBang.TypeSystem.ConstraintDatabase
( module X
) where

import Language.TinyBang.TypeSystem.ConstraintDatabase.Interface as X
import Language.TinyBang.TypeSystem.ConstraintDatabase.Simple as X
import Language.TinyBang.TypeSystem.ConstraintDatabase.Utils as X
