{-|
  This module contains the logging operations supported by TinyBang's logging
  system.  These operations are very simple and are not intended to be used
  directly.  Callers will typically want to generate module-specific functions
  via the @Language.TinyBang.Logging.Generators@ module.
-}
module Language.TinyBang.Utils.Logger.Operations
( logI
, logM
, bracketLog
, bracketLogM
) where

import Control.DeepSeq
import System.IO.Unsafe
import System.Log
import qualified System.Log.Logger

{-
WARNING: This logging module makes use of unsafePerformIO to prevent polluting
the entire codebase with a logging monad.  As a result, it is *very* fragile
and, if modified incorrectly, is subject to subtle deadlocking problems.  Please
be very careful when modifying.
-}

-- |A function, similar to @Debug.Trace.trace@, to log a messge in TinyBang in
--  an inline fashion.
logI :: String -- ^The name of the module doing the logging.
     -> Priority -- ^The logging level.
     -> String -- ^The message to log.
     -> a -- ^The value to use as the result of the logging expression.
     -> a
logI moduleName logLevel message =
  deepseq message $
  unsafePerformIO $! do
    System.Log.Logger.logM moduleName logLevel message
    return id
  
-- |A function to log a message in a monad.  *Note:* This routine is only
--  helpful if the monad is sufficiently strict to pull on the resulting unit.
--  A monad under @EitherT@ is strict enough; a monad under @ReaderT@ or
--  @StateT@ is not.
logM :: (Monad m) => String -> Priority -> String -> m ()
logM moduleName logLevel message =
  logI moduleName logLevel message $ return ()

-- |A bracket logging function designed for wrapping around a computation.
--  It expects a logging routine such as @_debugI@.  The start message is
--  generic; the end message is based on the reuslt of computation.  The "end"
--  of this bracket occurs whenever the data on which the end logging message
--  function pulls has been computed.
bracketLog :: (String -> a -> a) -> String -> (a -> String) -> a -> a
bracketLog logFn startMsg endMsgFn val =
  let result = logFn startMsg val in logFn (endMsgFn result) result

bracketLogM :: (Monad m)
            => (String -> m a -> m a) -> String -> (a -> String) -> m a -> m a
bracketLogM logFn startMsg endMsgFn val = do
  result <- logFn startMsg val
  logFn (endMsgFn result) $ return result
