{-# LANGUAGE ExistentialQuantification, GADTs, ViewPatterns #-}

{-|
  Defines the tokens used in the TinyBang parser.
-}

module Language.TinyBang.Syntax.Tokens
( Token(..)
, TokenBody
, SomeToken(..)
, TokenType(..)
, token
) where

import Language.TinyBang.Syntax.Location
import Language.TinyBang.Syntax.Positional
import Language.TinyBang.Utils.Display

data Token
  = forall a. Token (SomeToken a)
  
token :: TokenType a -> SourceSpan -> a -> Token
token tt ss a = Token (SomeToken tt (spos ss a))
  
type TokenBody a = SPositional a

data SomeToken a
  = SomeToken (TokenType a) (TokenBody a)

data TokenType a where
  TokIs :: TokenType () -- @=@
  TokArrow :: TokenType () -- @->@
  TokStartBlock :: TokenType () -- @{@
  TokStopBlock :: TokenType () -- @}@
  TokEmptyOnion :: TokenType () -- @()@
  TokOnion :: TokenType () -- @&@
  TokInt :: TokenType () -- @int@
  TokSemi :: TokenType () -- @;@
  TokIdentifier :: TokenType String
  TokLitInt :: TokenType Integer
  TokLabel :: TokenType String -- The @String@ is only the name of the label, not the @`@
  TokPlus :: TokenType () -- @+@
  TokMinus :: TokenType () -- @-@
  TokEq :: TokenType () -- @==@
  TokLessEq :: TokenType () -- @<=@
  TokGreaterEq :: TokenType () -- @>=@
  TokSet :: TokenType () -- @<-@
  TokRef :: TokenType () -- @ref@

instance Display Token where
  makeDoc t = case t of
    Token (SomeToken TokIs _) -> dquotes $ text "="
    Token (SomeToken TokEmptyOnion _) -> dquotes $ text "()"
    Token (SomeToken TokOnion _) -> dquotes $ text "&"
    Token (SomeToken TokArrow _) -> dquotes $ text "->"
    Token (SomeToken TokInt _) -> dquotes $ text "int"
    Token (SomeToken TokSemi _) -> dquotes $ text ";"
    Token (SomeToken TokStartBlock _) -> dquotes $ text "{"
    Token (SomeToken TokStopBlock _) -> dquotes $ text "}"
    Token (SomeToken TokIdentifier (posData -> s)) -> text "id#" <> dquotes (text s)
    Token (SomeToken TokLitInt (posData -> n)) -> text "int#" <> dquotes (text $ show n)
    Token (SomeToken TokLabel (posData -> n)) -> text "label#" <> dquotes (text n)
    Token (SomeToken TokPlus _) -> text "+"
    Token (SomeToken TokMinus _) -> text "-"
    Token (SomeToken TokEq _) -> text "=="
    Token (SomeToken TokLessEq _) -> text "<="
    Token (SomeToken TokGreaterEq _) -> text ">="
    Token (SomeToken TokSet _) -> text "<-"
    Token (SomeToken TokRef _) -> text "ref"
