{-|
  This module contains a typeclass which is used in @Display@ instances to
  prevent a cyclic dependency between the @Types@ and @Constraints@ modules.
-}
module Language.PatBang.TypeSystem.Utils.DocumentContainer
( DocumentContainer(..)
) where

import Language.PatBang.Display

class DocumentContainer a where
  getContainedDocuments :: a -> [Doc]

