{-|
  This module provides a mechanism by which the display properties of the
  tokens defined in @Language.TinyBang.Utils.Syntax.Tokens@ can be externalized.
  These types of tokens use an existential type which must be displayable for
  the overall token to have a displayed representation of its payload.  This
  module provides a typeclass which allows the definition, on a
  token-type-specific basis, of the display characteristics of token payloads.
-}
module Language.TinyBang.Utils.Syntax.TokenDisplay
( TokenDisplay(..)
) where

import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Syntax.Tokens

class TokenDisplay t where
  tokenPayloadDoc :: TypedToken t -> Doc
