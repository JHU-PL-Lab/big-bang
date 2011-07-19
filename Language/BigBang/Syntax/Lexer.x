{
module Language.BigBang.Syntax.Lexer
( Token(..)
, lexBigBang
) where
}

%wrapper "basic"

$digit = 0-9
$lowerAlpha = [a-z]
$upperAlpha = [A-Z]
$alpha = [$lowerAlpha $upperAlpha]

tokens :-

    $white+                             ;
    `                                   { const TokLabelPrefix }
    &                                   { const TokOnionCons }
    \\                                  { const TokLambda }
    fun                                 { const TokFun }
    \->                                 { const TokArrow }
    case                                { const TokCase }
    of                                  { const TokOf }
    int                                 { const TokInteger }
    char                                { const TokChar }
    unit                                { const TokUnit }
    \(                                  { const TokOpenParen }
    \)                                  { const TokCloseParen }
    $digit+                             { TokIntegerLiteral . read }
    \' ( \\. | ~\' ) \'                 { TokCharLiteral . head . tail }
    $alpha [$alpha $digit _ ']*         { TokIdentifier }
    \{                                  { const TokOpenBlock }
    \}                                  { const TokCloseBlock }
    \;                                  { const TokSeparator }

{
lexBigBang :: String -> [Token]
lexBigBang = alexScanTokens

data Token =
      TokLabelPrefix
    | TokOnionCons
    | TokLambda
    | TokFun
    | TokArrow
    | TokCase
    | TokOf
    | TokInteger
    | TokChar
    | TokUnit
    | TokOpenParen
    | TokCloseParen
    | TokIntegerLiteral Integer
    | TokCharLiteral Char
    | TokIdentifier String
    | TokOpenBlock
    | TokCloseBlock
    | TokSeparator
    deriving (Eq, Show)
}
    
