{-# LANGUAGE ExistentialQuantification, GADTs #-}

{-|
  Provides a general format for tokens used by the @Utils.Syntax@ module
  hierarchy.  A user of this module simply needs to define a GADT with
  constructors describing token types and a single type parameter describing
  the data payload for that type of token, e.g.
  @
      data TokenType a where
        TokInt :: TokenType Int
        TokPlus :: TokenType ()
        TokMinus :: TokenType ()
  @
  Tokens can then be created via the smart constructor @token@ by providing a
  type, a source span, and a data payload.  Users will often want to create a
  type alias for tokens carrying their particular type, e.g.
  @
      type Token = TypedToken TokenType
  @
-}
module Language.TinyBang.Utils.Syntax.Tokens
( TypedToken(..)
, token
, TokenBody
, SomeToken(..)
) where

import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Positional

data TypedToken t
  = forall a. Token (SomeToken t a)
  
token :: t a -> SourceSpan -> a -> TypedToken t
token tt ss a = Token (SomeToken tt (spos ss a))
  
type TokenBody a = SPositional a

data SomeToken t a
  = SomeToken (t a) (TokenBody a)
