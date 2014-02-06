{-# LANGUAGE TemplateHaskell #-}

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
, assertWithMessage
) where

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import System.IO.Unsafe

import Language.TinyBang.Utils.Logger

$(loggingFunctions)

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

-- |Asserts a condition.  If this program was compiled with GHC assertions and
--  the 'enableAssertions' routine has been called, this function will halt the
--  program when the provided boolean is @False@.  Otherwise, this function
--  ignores the argument and acts as identity.
assert :: Bool -> a -> a
assert b =
  E.assert $
    -- Using if-then-else rather than short-circuiting operators here to make
    -- our assumptions explicit.
    if unsafePerformIO $ join $ readIORef <$> assertionState
      then b
      else True {- assertions are disabled; don't evaluate the check -}

-- |Asserts a condition under the same circumstances as 'assert'.  If the
--  assertion is raised, however, an error is logged immediately before the
--  halt.  If assertions are not active, neither the message nor the boolean
--  are evaluated.
assertWithMessage :: String -> Bool -> a -> a
assertWithMessage msg b =
  E.assert $
    -- Using if-then-else rather than short-circuiting operators here to make
    -- our assumptions explicit.
    if unsafePerformIO $ join $ readIORef <$> assertionState
      then
        if b
          then True -- Assertion successful
          else _errorI ("Assertion failure: " ++ msg) False -- Log then fail
      else True -- assertions are disabled; don't evaluate the check
