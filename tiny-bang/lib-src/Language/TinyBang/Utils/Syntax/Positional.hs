{-# LANGUAGE FlexibleInstances #-}

{-|
  This module defines a "positional" functor: a container which may tag data
  with a source location.
-}
module Language.TinyBang.Utils.Syntax.Positional
( Positional(..)
, Void(..)
, SPositional
, VPositional
, posData
, posSpan
, spos
, vpos
, forgetSpan
, posOver
) where

import Data.Functor.Identity

import Language.TinyBang.Utils.Syntax.Location

-- |The type for "positional" data.  Positional data may or may not have a
--  @SourceSpan@ associated with it.  This association is used to provide spans
--  for the parsed AST nodes.  The expected values for @f@ are either @Identity@
--  or @Void@.
newtype Positional f a = Positional (f SourceSpan, a)

instance Functor (Positional f) where
  fmap f (Positional (s,d)) = Positional (s, f d)

instance HasSourceSpan (Positional Identity a) where
  spanOf = posSpan

-- |Used to discard type arguments.
newtype Void a = Void ()

-- |A type alias for spanned positional data.
type SPositional = Positional Identity 

-- |A type alias for unspanned positional data.
type VPositional = Positional Void

-- |Retrieves the payload from a positional value.
posData :: Positional f a -> a
posData (Positional (_, d)) = d

-- |Retrieves the span from a spanned positional value.
posSpan :: Positional Identity a -> SourceSpan
posSpan (Positional (Identity s, _)) = s

-- |A smart constructor for spanned positional data.
spos :: SourceSpan -> a -> SPositional a
spos s x = Positional (Identity s, x)

-- |A smart constructor for unspanned positional data.
vpos :: a -> VPositional a
vpos x = Positional (Void (), x)

-- |A conversion from spanned data to unspanned data.
forgetSpan :: SPositional a -> VPositional a
forgetSpan = vpos . posData

-- |A convenience mechanism for creating a positional over two things which have
--  source spans.
posOver :: (HasSourceSpan s1, HasSourceSpan s2)
        => s1 -> s2 -> a -> SPositional a
posOver s1 s2 = spos (spanOf s1 <--> spanOf s2)
