module Language.TinyBang.Syntax.NewLexer
( Token(..)
, lexTinyBang
, LexerResult
) where

import Text.ParserCombinators.Parsec
import Utils.Render.Display --(Display, makeDoc)


lexer :: Parser [Token]
lexer = undefined

lexTinyBang :: String -> LexerResult
lexTinyBang s = case parse lexer "" s of
    Left x -> Left (show x)
    Right x -> Right x

type LexerResult = Either String [Token]

data Token =
      TokLabelPrefix
    | TokOnionCons
    | TokOnionSub
    | TokOnionProj
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
    | TokColon
    | TokDef
    | TokEquals
    | TokIn
    | TokOpPlus
    | TokOpMinus
    | TokOpEquals
    | TokOpLessEquals
    | TokOpGreaterEquals
    deriving (Eq, Show)

instance Display Token where
    makeDoc tok = text $ case tok of
        TokLabelPrefix -> "label prefix"
        TokOnionCons -> "onion constructor"
        TokOnionSub -> "onion subtractor"
        TokOnionProj -> "onion projector"
        TokLambda -> "lambda"
        TokFun -> "fun"
        TokArrow -> "arrow"
        TokCase -> "case"
        TokOf -> "of"
        TokInteger -> "int"
        TokChar -> "char"
        TokUnit -> "unit"
        TokOpenParen -> "open parenthesis"
        TokCloseParen -> "close parenthesis"
        TokIntegerLiteral _ -> "int literal"
        TokCharLiteral _ -> "char literal"
        TokIdentifier _ -> "identifier"
        TokOpenBlock -> "open block"
        TokCloseBlock -> "close block"
        TokSeparator -> "separator"
        TokColon -> "colon"
        TokDef -> "def"
        TokEquals -> "equals"
        TokIn -> "in"
        TokOpPlus -> "op plus"
        TokOpMinus -> "op minus"
        TokOpEquals -> "op equals"
        TokOpLessEquals -> "op less than or equal"
        TokOpGreaterEquals -> "op greater than or equal"
