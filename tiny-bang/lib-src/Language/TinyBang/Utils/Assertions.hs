{-|
  A module for TinyBang assertion management.  There are two mechanisms that
  control whether TinyBang assertions are active.  The first is a static
  component: TinyBang must be compiled using the @-fno-ignore-assertions@ flag
  because it uses @Control.Exception.assert@.  The second is dynamic; a global
  (unsafe) configuration is used to determine whether or not assertions are
  active.  If assertions are (1) not compiled in or (2) not actived by this
  dynamic mechanism (e.g. by command-line argument processing), every call to
  this module's 'assert' is a no-op.
-}
module Language.TinyBang.Utils.Assertions
( enableAssertions
, assert
) where

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import System.IO.Unsafe

assertionState :: IO (IORef Bool)
assertionState = newIORef False

staticAssertionsEnabled :: IO Bool
staticAssertionsEnabled =
  E.catch (assert False $ return False) (\(E.AssertionFailed _) -> return True)

enableAssertions :: IO ()
enableAssertions = do
  sae <- staticAssertionsEnabled
  if sae
    then join $ writeIORef <$> assertionState <*> return True
    else ioError $ userError
            "Attempted to enable assertions, but assertions not compiled in."

assert :: Bool -> a -> a
assert b = E.assert (b && unsafePerformIO (join $ readIORef <$> assertionState))
