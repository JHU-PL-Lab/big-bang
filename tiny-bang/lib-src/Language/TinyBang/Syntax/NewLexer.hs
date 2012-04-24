module Language.TinyBang.Syntax.NewLexer
( Token(..)
, lexTinyBang
, LexerResult
) where

import Text.ParserCombinators.Parsec
import Utils.Render.Display (Display, makeDoc, text)


lexer :: Parser [Token]
lexer = undefined --hungry, reservedWords, shortOperators, longOperators, whitespaceP
    where
        whitespaceP :: Parser ()
        whitespaceP = skipMany (oneOf " \n\t")

hungry :: [Parser Token]
hungry = [identP, intLiteralP, charLiteralP]
    where
        identP = do
            first <- letter <|> char '_'
            rest <- many (letter <|> digit <|> oneOf "_'")
            return $ TokIdentifier (first:rest)
        intLiteralP = do
            digits <- many digit
            return $ TokIntegerLiteral (read digits)
        charLiteralP = do
            _ <- char '\''
            l <- letter
            _ <- char '\''
            return $ TokCharLiteral l

shortOperators :: [Parser Token]
shortOperators =
    [ lblPrefixP
    , lambdaP
    , openParenP
    , closeParenP
    , openBlockP
    , closeBlockP
    , separatorP
    , colonP
    , opPlusP
    , opMinusP
    , equalsP
    , onionConsP]
    where
        lblPrefixP = do
            _ <- char '`'
            return TokLabelPrefix
        lambdaP = do
            _ <- char '\\'
            return TokLambda
        openParenP = do
            _ <- char '('
            return TokOpenParen
        closeParenP = do
            _ <- char ')'
            return TokCloseParen
        openBlockP = do
            _ <- char '{'
            return TokOpenBlock
        closeBlockP = do
            _ <- char '}'
            return TokCloseBlock
        separatorP = do
            _ <- char ';'
            return TokSeparator
        colonP = do
            _ <- char ':'
            return TokColon
        opPlusP = do
            _ <- char '+'
            return TokOpPlus
        opMinusP = do
            _ <- char '-'
            return TokOpMinus
        equalsP = do
            _ <- char '='
            return TokEquals
        onionConsP = do
            _ <- char '&'
            return TokOnionCons

longOperators :: [Parser Token]
longOperators = [onionSubP, onionProjP, arrowP, opEqualsP, opLessEqualsP, opGreaterEqualsP]
    where
        onionSubP = do
            _ <- string "&-"
            return TokOnionSub
        onionProjP = do
            _ <- string "&."
            return TokOnionProj
        arrowP = do
            _ <- string "->"
            return TokArrow
        opEqualsP = do
            _ <- string "=="
            return TokOpEquals
        opLessEqualsP = do
            _ <- string "<="
            return TokOpLessEquals
        opGreaterEqualsP = do
            _ <- string ">="
            return TokOpGreaterEquals


reservedWords :: [Parser Token]
reservedWords = [funP, caseP, ofP, intP, charP, unitP, defP,inP, finalP,immutP]
    where
        funP = do
            _ <- string "fun"
            return TokFun
        caseP = do
            _ <- string "case"
            return TokCase
        ofP = do
            _ <- string "of"
            return TokOf
        intP = do
            _ <- string "int"
            return TokInteger
        charP = do
            _ <- string "char"
            return TokChar
        unitP = do
            _ <- string "unit"
            return TokUnit
        defP = do
            _ <- string "def"
            return TokDef
        inP = do
            _ <- string "in"
            return TokIn
        finalP = do
            _ <- string "final"
            return TokFinal
        immutP = do
            _ <- string "immut"
            return TokImmut


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
    | TokFinal
    | TokImmut
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
        TokFinal -> "final"
        TokImmut -> "immut"
