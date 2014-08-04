{-# LANGUAGE ExistentialQuantification, GADTs, ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}

{-|
  Defines the tokens used in the LittleBang parser.
-}

module Language.LittleBang.Syntax.Tokens
( Token
, TokenType(..)
) where

import Language.TinyBang.Utils.Display
import Language.TinyBang.Utils.Syntax

type Token = TypedToken TokenType

data TokenType a where
  TokLet :: TokenType () -- @let@
  TokIs :: TokenType () -- @=@
  TokIn :: TokenType () -- @in@
  TokLambda :: TokenType () -- @fun@
  TokArrow :: TokenType () -- @->@
  TokIf :: TokenType () -- @if@
  TokThen :: TokenType () -- @then@
  TokElse :: TokenType () -- @else@
  TokPlus :: TokenType () -- @+@
  TokMinus :: TokenType () -- @-@
  TokEq :: TokenType () -- @==@
  TokLessEq :: TokenType () -- @<=@
  TokGreaterEq :: TokenType () -- @>=@
  TokSet :: TokenType () -- @<-@
  TokOnion :: TokenType () -- @&@
  TokOpenParen :: TokenType () -- @(@
  TokCloseParen :: TokenType () -- @)@
  TokOpenBracket :: TokenType () -- @[@
  TokCloseBracket :: TokenType () -- @]@
  TokComma :: TokenType () -- @,@
  TokColon :: TokenType () -- @:@
  TokSemi :: TokenType () -- @;@
  TokRef :: TokenType () -- @ref@
  TokEmptyOnion :: TokenType () -- @()@
  TokInt :: TokenType () -- @int@  
  TokIdentifier :: TokenType String
  TokLitInt :: TokenType Integer
  TokLabel :: TokenType String -- The @String@ is only the name of the label, not the @`@
  TokDeref :: TokenType () -- @!@
  TokCons :: TokenType () -- @::@
  -- TODO: what about list patterns (e.g. "...")?

instance TokenDisplay TokenType where
  tokenPayloadDoc t = case t of
    Token (SomeToken TokLet _) -> dquotes $ text "let"
    Token (SomeToken TokIs _) -> dquotes $ text "="
    Token (SomeToken TokIn _) -> dquotes $ text "in"
    Token (SomeToken TokLambda _) -> dquotes $ text "fun"
    Token (SomeToken TokArrow _) -> dquotes $ text "->"
    Token (SomeToken TokIf _) -> dquotes $ text "if"
    Token (SomeToken TokThen _) -> dquotes $ text "then"
    Token (SomeToken TokElse _) -> dquotes $ text "else"
    Token (SomeToken TokPlus _) -> text "+"
    Token (SomeToken TokMinus _) -> text "-"
    Token (SomeToken TokEq _) -> text "=="
    Token (SomeToken TokLessEq _) -> text "<="
    Token (SomeToken TokGreaterEq _) -> text ">="
    Token (SomeToken TokSet _) -> text "<-"
    Token (SomeToken TokOnion _) -> dquotes $ text "&"
    Token (SomeToken TokOpenParen _) -> dquotes $ text "("
    Token (SomeToken TokCloseParen _) -> dquotes $ text ")"
    Token (SomeToken TokOpenBracket _) -> dquotes $ text "["
    Token (SomeToken TokCloseBracket _) -> dquotes $ text "]"
    Token (SomeToken TokComma _) -> dquotes $ text ","
    Token (SomeToken TokColon _) -> dquotes $ text ":"
    Token (SomeToken TokSemi _) -> dquotes $ text ";"
    Token (SomeToken TokRef _) -> text "ref"
    Token (SomeToken TokEmptyOnion _) -> dquotes $ text "()"
    Token (SomeToken TokInt _) -> dquotes $ text "int"
    Token (SomeToken TokIdentifier (posData -> s)) -> text "id#" <> dquotes (text s)
    Token (SomeToken TokLitInt (posData -> n)) -> text "int#" <> dquotes (text $ show n)
    Token (SomeToken TokLabel (posData -> n)) -> text "label#" <> dquotes (text n)
    Token (SomeToken TokDeref _) -> text "!"
    Token (SomeToken TokCons _) -> text "::"

instance Display Token where
  makeDoc = tokenPayloadDoc
