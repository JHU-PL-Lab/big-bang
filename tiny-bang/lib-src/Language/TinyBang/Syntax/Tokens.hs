{-# LANGUAGE ExistentialQuantification, GADTs, ViewPatterns #-}

{-|
  Defines the tokens used in the TinyBang parser.
-}

module Language.TinyBang.Syntax.Tokens
( Token
, TokenType(..)
) where

import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Syntax

type Token = TypedToken TokenType

data TokenType a where
  TokEOF :: TokenType ()
  TokIs :: TokenType () -- @=@
  TokArrow :: TokenType () -- @->@
  TokBackslash :: TokenType () -- @\@
  TokStartBlock :: TokenType () -- @{@
  TokStopBlock :: TokenType () -- @}@
  TokEmptyOnion :: TokenType () -- @()@
  TokOnion :: TokenType () -- @&@
  TokInt :: TokenType () -- @int@
  TokChar :: TokenType () -- @char@
  TokSemi :: TokenType () -- @;@
  TokIdentifier :: TokenType String
  TokLitInt :: TokenType Integer
  TokLitChar :: TokenType Char
  TokLabel :: TokenType String -- The @String@ is only the name of the label, not the @`@
  TokPlus :: TokenType () -- @+@
  TokMinus :: TokenType () -- @-@
  TokAsterisk :: TokenType () -- @*@
  TokDiv :: TokenType () -- @/@
  TokMod :: TokenType () -- @%@
  TokEq :: TokenType () -- @==@
  TokLessEq :: TokenType () -- @<=@
  TokGreaterEq :: TokenType () -- @>=@
  TokSet :: TokenType () -- @<-@
  TokRef :: TokenType () -- @ref@
  TokGetChar :: TokenType () -- @getChar@
  TokPutChar :: TokenType () -- @putChar@

instance TokenDisplay TokenType where
  tokenPayloadDoc t = case t of
    Token (SomeToken TokEOF _) -> dquotes $ text "<EOF>"
    Token (SomeToken TokIs _) -> dquotes $ text "="
    Token (SomeToken TokEmptyOnion _) -> dquotes $ text "()"
    Token (SomeToken TokOnion _) -> dquotes $ text "&"
    Token (SomeToken TokArrow _) -> dquotes $ text "->"
    Token (SomeToken TokInt _) -> dquotes $ text "int"
    Token (SomeToken TokChar _) -> dquotes $ text "char"
    Token (SomeToken TokSemi _) -> dquotes $ text ";"
    Token (SomeToken TokBackslash _) -> dquotes $ text "\\"
    Token (SomeToken TokStartBlock _) -> dquotes $ text "{"
    Token (SomeToken TokStopBlock _) -> dquotes $ text "}"
    Token (SomeToken TokIdentifier (posData -> s)) -> text "id#" <> dquotes (text s)
    Token (SomeToken TokLitInt (posData -> n)) -> text "int#" <> dquotes (text $ show n)
    Token (SomeToken TokLitChar (posData -> n)) -> text "char#" <> dquotes (text $ show n)
    Token (SomeToken TokLabel (posData -> n)) -> text "label#" <> dquotes (text n)
    Token (SomeToken TokPlus _) -> text "+"
    Token (SomeToken TokMinus _) -> text "-"
    Token (SomeToken TokAsterisk _) -> text "*"
    Token (SomeToken TokDiv _) -> text "/"
    Token (SomeToken TokMod _) -> text "%"
    Token (SomeToken TokEq _) -> text "=="
    Token (SomeToken TokLessEq _) -> text "<="
    Token (SomeToken TokGreaterEq _) -> text ">="
    Token (SomeToken TokSet _) -> text "<-"
    Token (SomeToken TokRef _) -> text "ref"
    Token (SomeToken TokGetChar _) -> text "getChar"
    Token (SomeToken TokPutChar _) -> text "putChar"
