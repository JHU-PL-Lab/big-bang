{-|
  This module implements the TinyBang constraint closure relation.
-}
module Language.TinyBang.TypeSystem.Closure
( ClosureError(..)
, calculateClosure
) where

import Language.TinyBang.TypeSystem.Types

-- |A data structure representing errors in constraint closure.
data ClosureError
  = TODOClosureError

-- |Calculates the transitive closure of the provided constraint database.  The
--  transitive closure of a TinyBang database is only confluent up to
--  equivalence of contour folding; this function will produce a representative
--  of the appropriate equivalence class.
calculateClosure :: (ConstraintDatabase db) => db -> Either ClosureError db
calculateClosure = undefined -- TODO
