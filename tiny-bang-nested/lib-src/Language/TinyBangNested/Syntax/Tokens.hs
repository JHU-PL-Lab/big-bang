module Language.TinyBangNested.Syntax.Tokens
( PositionalToken(..)
, Token(..)
) where

import Language.TinyBang.Syntax.Location
import Language.TinyBang.Utils.Display

-- |The raw tokens of the TBN syntax.
data Token
  = TokIs -- ^@=@
  | TokThrows -- ^@throws@
  | TokPlus -- ^@+@
  | TokMinus -- ^@-@
  | TokEq -- ^@==@
  | TokLessEq -- ^@<=@
  | TokGreaterEq -- ^@>=@
  | TokEmptyOnion -- ^@()@
  | TokOnion -- ^@&@
  | TokArrow -- ^@->@
  | TokLet -- ^@let@
  | TokIn -- ^@in@
  | TokImmut -- ^@immut@
  | TokFinal -- ^@final@
  | TokExn -- ^@exn@
  | TokFun -- ^@fun@
  | TokInt -- ^@int@
  | TokOpenParen -- ^@(@
  | TokCloseParen -- ^@)@
  | TokIdentifier String
  | TokLitInt Integer
  | TokLitChar Char
  | TokLabel String -- ^The @String@ is only the name of the label, not the @`@
  deriving (Eq, Ord, Show)
  
-- |An annotation for tokens which describes their source position.
data PositionalToken
  = PositionalToken { startPos :: DocumentPosition
                    , stopPos :: DocumentPosition
                    , posToken :: Token }
  deriving (Eq, Ord, Show)

instance HasDocumentStartStopPositions PositionalToken where
  documentStartPositionOf = startPos
  documentStopPositionOf = stopPos
  
instance Display Token where
  makeDoc t = case t of
    TokIs -> dquotes $ text "="
    TokThrows -> dquotes $ text "throws"
    TokPlus -> dquotes $ text "+"
    TokMinus -> dquotes $ text "-"
    TokLessEq -> dquotes $ text "<="
    TokGreaterEq -> dquotes $ text ">="
    TokEq -> dquotes $ text "=="
    TokEmptyOnion -> dquotes $ text "()"
    TokOnion -> dquotes $ text "&"
    TokArrow -> dquotes $ text "->"
    TokLet -> dquotes $ text "let"
    TokIn -> dquotes $ text "in"
    TokImmut -> dquotes $ text "immut"
    TokFinal -> dquotes $ text "final"
    TokExn -> dquotes $ text "exn"
    TokFun -> dquotes $ text "fun"
    TokInt -> dquotes $ text "int"
    TokOpenParen -> dquotes $ text "("
    TokCloseParen -> dquotes $ text ")"
    TokIdentifier s -> text "id#" <> dquotes (text s)
    TokLitInt n -> text "int#" <> dquotes (text $ show n)
    TokLitChar c -> text "char#" <> dquotes (text [c])
    TokLabel n -> text "label#" <> dquotes (text n)

instance Display PositionalToken where
  makeDoc pt =
    makeDoc (posToken pt) <+> text "at" <+>
      makeDoc (startPos pt) <> char '-' <> makeDoc (stopPos pt)
