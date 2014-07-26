module Test.TinyBang.TestUtils.ExpectDsl.Tokens
( Token(..)
) where

import Language.TinyBang.Utils.Display

data Token
  = TokEmptyOnion -- @()@
  | TokOnion -- @&@
  | TokOpenParen -- @(@
  | TokCloseParen -- @)@
  | TokLitInt Integer
  | TokLabel String  -- The @String@ is only the name of the label, not the @`@
  | TokRef -- @ref@
  | TokScapes -- @scapes@
  | TokTypeFail -- @typefail@
  | TokEOF

instance Display Token where
  makeDoc t = case t of
    TokEmptyOnion -> dquotes $ text "()"
    TokOnion -> dquotes $ text "&"
    TokOpenParen -> dquotes $ text "("
    TokCloseParen -> dquotes $ text ")"
    TokLitInt n -> text "int#" <> dquotes (text $ show n)
    TokLabel n -> text "label#" <> dquotes (text n)
    TokRef -> text "ref"
    TokScapes -> text "scapes"
    TokTypeFail -> text "typefail"
    TokEOF -> text "<<EOF>>"