module Language.TinyBang.Syntax.NewLexer
( Token(..)
, lexTinyBang
, LexerResult
) where

import Text.ParserCombinators.Parsec
import Utils.Render.Display (Display, makeDoc, text)


lexer :: Parser [Token]
lexer = do
    toks <- many (whitespaceP <|> thing)
    return (concat toks)
    where
        thing = do
            tok <- tryThemAll
            return [tok]
        tryThemAll = let tries = map (try) $ concat [reservedWords, longOperators, hungry, shortOperators] in
            foldl (<|>) (head tries) (tail tries)

whitespaceP :: Parser [Token]
whitespaceP = do
    _ <- oneOf " \n\t"
    skipMany (oneOf " \n\t")
    return []

validIdentP = notFollowedBy (alphaNum <|> oneOf "_'")

hungry :: [Parser Token]
hungry = [identP, intLiteralP, charLiteralP]
    where
        identP = do
            first <- letter <|> char '_'
            rest <- many (letter <|> digit <|> oneOf "_'")
            return $ TokIdentifier (first:rest)
        intLiteralP = do
            prefix <- option ' ' (char '-')
            first <- digit
            digits <- many digit
            validIdentP
            return $ TokIntegerLiteral (read (prefix:(first:digits)))
        charLiteralP = do
            _ <- char '\''
            l <- anyChar
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
reservedWords =  [funP, caseP, ofP, intP, charP, unitP, defP,inP, finalP,immutP]
    where
        funP = do
            _ <- string "fun"
            validIdentP
            return TokFun
        caseP = do
            _ <- string "case"
            validIdentP
            return TokCase
        ofP = do
            _ <- string "of"
            validIdentP
            return TokOf
        intP = do
            _ <- string "int"
            validIdentP
            return TokInteger
        charP = do
            _ <- string "char"
            validIdentP
            return TokChar
        unitP = do
            _ <- string "unit"
            validIdentP
            return TokUnit
        defP = do
            _ <- string "def"
            validIdentP
            return TokDef
        inP = do
            _ <- string "in"
            validIdentP
            return TokIn
        finalP = do
            _ <- string "final"
            validIdentP
            return TokFinal
        immutP = do
            _ <- string "immut"
            validIdentP
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
