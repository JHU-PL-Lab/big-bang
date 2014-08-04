{-|
  This module contains utilities to ease the construction of a Happy parser
  which uses the token style from @Language.TinyBang.Utils.Syntax.Tokens@ to
  create span-tracking structures.
-}
module Language.TinyBang.Utils.Syntax.Parser
( ParserM
, parseError

, oc0
, oc1
, oc2
, oc3

, oc1S
, oat
) where

import Language.TinyBang.Ast.Origin
import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Syntax.Location
import Language.TinyBang.Utils.Syntax.Positional
import Language.TinyBang.Utils.Syntax.TokenDisplay
import Language.TinyBang.Utils.Syntax.Tokens

-- * A parsing monad

-- |A simple monad type for Happy parsers.  This section also contains utilities
--  meant to operate within this monad.
type ParserM a = Either String a

-- |A simple function for generating a Happy parse error when using the parser
--  monad given here and the token types given in this module.
parseError :: (TokenDisplay t) => [TypedToken t] -> ParserM a
parseError tokens =
  case tokens of
    [] -> Left $ "Unexpected end of file"
    (t@(Token (SomeToken _ body))):_ ->
      let pos = posSpan body in
      Left $ display $
        makeDoc pos <> char ':' <+> text "Unexpected token:" <+> tokenPayloadDoc t

-- * Utility functions

-- |A version of @oc2@ for constructors with /no/ additional arguments rather
--  than two.
oc0 :: SPositional s1
    -> SPositional s2
    -> (Origin -> r)
    -> SPositional r
oc0 s1 s2 f =
  let ss = posSpan s1 <--> posSpan s2 in
  spos ss $ f (SourceOrigin ss)

-- |A version of @oc2@ for constructors with /one/ additional argument rather
--  than two.
oc1 :: SPositional s1
    -> SPositional s2
    -> (Origin -> a1 -> r)
    -> Positional f1 a1
    -> SPositional r
oc1 s1 s2 f a1 =
  let ss = posSpan s1 <--> posSpan s2 in
  spos ss $ f (SourceOrigin ss) (posData a1)

-- |A tool for calling constructors which expect origins within this parser.
--  This function accepts the first and last token of the parse, a constructor
--  expecting an @Origin@ argument followed by two other arguments, and the
--  @Positional@ form of those two arguments.  This function then creates an
--  @Origin@ based on the provided tokens and calls the constructor with this
--  and the data parts of the remaining arguments.  The result is the positional
--  form of the result of the constructor.
oc2 :: SPositional s1
    -> SPositional s2
    -> (Origin -> a1 -> a2 -> r)
    -> Positional f1 a1
    -> Positional f2 a2
    -> SPositional r
oc2 s1 s2 f a1 a2 =
  let ss = posSpan s1 <--> posSpan s2 in
  spos ss $ f (SourceOrigin ss) (posData a1) (posData a2)

-- |A version of @oc2@ for constructors with /one/ additional argument rather
--  than two.
oc3 :: SPositional s1
    -> SPositional s2
    -> (Origin -> a1 -> a2 -> a3 -> r)
    -> Positional f1 a1
    -> Positional f2 a2
    -> Positional f3 a3
    -> SPositional r
oc3 s1 s2 f a1 a2 a3 =
  let ss = posSpan s1 <--> posSpan s2 in
  spos ss $ f (SourceOrigin ss) (posData a1) (posData a2) (posData a3)

-- |A utility for handling tokens which represent single-token AST subtrees.
oc1S :: (Origin -> a -> r) -> SPositional a -> SPositional r
oc1S f a =
    spos (posSpan a) $ f (SourceOrigin $ posSpan a) (posData a)

-- |A version of @at@ which takes a constructor expecting an origin and no other
--  arguments.
oat :: (Origin -> a) -> SPositional b -> SPositional a
oat f b = spos (posSpan b) $ f (SourceOrigin (posSpan b))
