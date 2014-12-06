{-|
  A naive implementation of the TinyBang type system.  This implementation is
  meant to be obviously correct at the cost of performance.  This way, it can be
  used as a gold standard for confirming the behavior of other implementations.
-}
module Language.TinyBang.TypeSystem.Simple
( module X
) where

import Language.TinyBang.TypeSystem.Simple.Data as X
import Language.TinyBang.TypeSystem.Simple.Typechecker as X
