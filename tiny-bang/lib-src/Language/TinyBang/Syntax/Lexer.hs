module Language.TinyBang.Syntax.Lexer
( Token(..)
, lexTinyBang
, LexerResult
, getPos
, weakEq
, SourceLocation
, defaultSourceLocation
) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos (initialPos)
import Utils.Render.Display (Display, makeDoc, text)

lexTinyBang :: String -> LexerResult
lexTinyBang s = case parse lexer "" s of
    Left x -> Left (show x)
    Right x -> Right $ map (\(start,t,e) -> t (start,e)) x


lexer :: Parser [(SourcePos, (SourceLocation -> Token), SourcePos)]
lexer = do
    toks <- many (whitespaceP <|> thing)
    eof
    return (concat toks)
    where
        thing = do
            start <- getPosition
            tok <- tryThemAll
            end <- getPosition
            return [(start,tok,end)]
        tryThemAll = let tries = map (try) $ concat [reservedWords, longOperators, hungry, shortOperators] in
            foldl (<|>) (head tries) (tail tries)

whitespaceP :: Parser [(SourcePos, (SourceLocation -> Token), SourcePos)]
whitespaceP = do
    _ <- oneOf " \n\t"
    skipMany (oneOf " \n\t")
    return []

validIdentP :: Parser ()
validIdentP = notFollowedBy (alphaNum <|> oneOf "_'")

hungry :: [Parser (SourceLocation -> Token)]
hungry = [identP, intLiteralP, charLiteralP]
    where
        identP = do
            first <- letter <|> char '_'
            rest <- many (letter <|> digit <|> oneOf "_'")
            validIdentP
            return $ (flip TokIdentifier) (first:rest)
        intLiteralP = do
            prefix <- option ' ' (char '-')
            first <- digit
            digits <- many digit
            return $ (flip TokIntegerLiteral) (read (prefix:(first:digits)))
        charLiteralP = do
            _ <- char '\''
            l <- anyChar
            _ <- char '\''
            return $ (flip TokCharLiteral) l

shortOperators :: [Parser (SourceLocation -> Token)]
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

longOperators :: [Parser (SourceLocation -> Token)]
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

reservedWords :: [Parser (SourceLocation -> Token)]
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

type LexerResult = Either String [Token]

type SourceLocation = (SourcePos, SourcePos)
defaultSourceLocation :: SourceLocation
defaultSourceLocation = ((initialPos ""),(initialPos ""))


data Token =
      TokLabelPrefix SourceLocation
    | TokOnionCons SourceLocation
    | TokOnionSub SourceLocation
    | TokOnionProj SourceLocation
    | TokLambda SourceLocation
    | TokFun SourceLocation
    | TokArrow SourceLocation
    | TokCase SourceLocation
    | TokOf SourceLocation
    | TokInteger SourceLocation
    | TokChar SourceLocation
    | TokUnit SourceLocation
    | TokOpenParen SourceLocation
    | TokCloseParen SourceLocation
    | TokIntegerLiteral SourceLocation Integer
    | TokCharLiteral SourceLocation Char
    | TokIdentifier SourceLocation String
    | TokOpenBlock SourceLocation
    | TokCloseBlock SourceLocation
    | TokSeparator SourceLocation
    | TokColon SourceLocation
    | TokDef SourceLocation
    | TokEquals SourceLocation
    | TokIn SourceLocation
    | TokOpPlus SourceLocation
    | TokOpMinus SourceLocation
    | TokOpEquals SourceLocation
    | TokOpLessEquals SourceLocation
    | TokOpGreaterEquals SourceLocation
    | TokFinal SourceLocation
    | TokImmut SourceLocation
    deriving (Show)

instance Eq Token where
    (==) t1 t2 = case (t1,t2) of
        (TokLabelPrefix _,TokLabelPrefix _) -> True
        (TokOnionCons _,TokOnionCons _) -> True
        (TokOnionSub _,TokOnionSub _) -> True
        (TokOnionProj _,TokOnionProj _) -> True
        (TokLambda _,TokLambda _) -> True
        (TokFun _,TokFun _) -> True
        (TokArrow _,TokArrow _) -> True
        (TokCase _,TokCase _) -> True
        (TokOf _,TokOf _) -> True
        (TokInteger _,TokInteger _) -> True
        (TokChar _,TokChar _) -> True
        (TokUnit _,TokUnit _) -> True
        (TokOpenParen _,TokOpenParen _) -> True
        (TokCloseParen _,TokCloseParen _) -> True
        (TokIntegerLiteral _ i1,TokIntegerLiteral _ i2) | i1 == i2 -> True
        (TokCharLiteral _ c1,TokCharLiteral _ c2) | c1 == c2 -> True
        (TokIdentifier _ i1,TokIdentifier _ i2) | i1 == i2 -> True
        (TokOpenBlock _,TokOpenBlock _) -> True
        (TokCloseBlock _,TokCloseBlock _) -> True
        (TokSeparator _,TokSeparator _) -> True
        (TokColon _,TokColon _) -> True
        (TokDef _,TokDef _) -> True
        (TokEquals _,TokEquals _) -> True
        (TokIn _,TokIn _) -> True
        (TokOpPlus _,TokOpPlus _) -> True
        (TokOpMinus _,TokOpMinus _) -> True
        (TokOpEquals _,TokOpEquals _) -> True
        (TokOpLessEquals _,TokOpLessEquals _) -> True
        (TokOpGreaterEquals _,TokOpGreaterEquals _) -> True
        (TokFinal _,TokFinal _) -> True
        (TokImmut _,TokImmut _) -> True
        (_,_) -> False -- they didn't match

weakEq :: Token -> Token -> Bool
weakEq t1 t2 = case (t1,t2) of
    (TokLabelPrefix _,TokLabelPrefix _) -> True
    (TokOnionCons _,TokOnionCons _) -> True
    (TokOnionSub _,TokOnionSub _) -> True
    (TokOnionProj _,TokOnionProj _) -> True
    (TokLambda _,TokLambda _) -> True
    (TokFun _,TokFun _) -> True
    (TokArrow _,TokArrow _) -> True
    (TokCase _,TokCase _) -> True
    (TokOf _,TokOf _) -> True
    (TokInteger _,TokInteger _) -> True
    (TokChar _,TokChar _) -> True
    (TokUnit _,TokUnit _) -> True
    (TokOpenParen _,TokOpenParen _) -> True
    (TokCloseParen _,TokCloseParen _) -> True
    (TokIntegerLiteral _ _,TokIntegerLiteral _ _) -> True
    (TokCharLiteral _ _,TokCharLiteral _ _) -> True
    (TokIdentifier _ _,TokIdentifier _ _) -> True
    (TokOpenBlock _,TokOpenBlock _) -> True
    (TokCloseBlock _,TokCloseBlock _) -> True
    (TokSeparator _,TokSeparator _) -> True
    (TokColon _,TokColon _) -> True
    (TokDef _,TokDef _) -> True
    (TokEquals _,TokEquals _) -> True
    (TokIn _,TokIn _) -> True
    (TokOpPlus _,TokOpPlus _) -> True
    (TokOpMinus _,TokOpMinus _) -> True
    (TokOpEquals _,TokOpEquals _) -> True
    (TokOpLessEquals _,TokOpLessEquals _) -> True
    (TokOpGreaterEquals _,TokOpGreaterEquals _) -> True
    (TokFinal _,TokFinal _) -> True
    (TokImmut _,TokImmut _) -> True
    (_,_) -> False -- they didn't match


instance Display Token where
    makeDoc tok = text $ case tok of
        TokLabelPrefix _ -> "label prefix"
        TokOnionCons _ -> "onion constructor"
        TokOnionSub _ -> "onion subtractor"
        TokOnionProj _ -> "onion projector"
        TokLambda _ -> "lambda"
        TokFun _ -> "fun"
        TokArrow _ -> "arrow"
        TokCase _ -> "case"
        TokOf _ -> "of"
        TokInteger _ -> "int"
        TokChar _ -> "char"
        TokUnit _ -> "unit"
        TokOpenParen _ -> "open parenthesis"
        TokCloseParen _ -> "close parenthesis"
        TokIntegerLiteral _ _ -> "int literal"
        TokCharLiteral _ _ -> "char literal"
        TokIdentifier _ _ -> "identifier"
        TokOpenBlock _ -> "open block"
        TokCloseBlock _ -> "close block"
        TokSeparator _ -> "separator"
        TokColon _ -> "colon"
        TokDef _ -> "def"
        TokEquals _ -> "equals"
        TokIn _ -> "in"
        TokOpPlus _ -> "op plus"
        TokOpMinus _ -> "op minus"
        TokOpEquals _ -> "op equals"
        TokOpLessEquals _ -> "op less than or equal"
        TokOpGreaterEquals _ -> "op greater than or equal"
        TokFinal _ -> "final"
        TokImmut _ -> "immut"

getPos :: Token -> SourceLocation
getPos t = case t of
    TokLabelPrefix p -> p
    TokOnionCons p -> p
    TokOnionSub p -> p
    TokOnionProj p -> p
    TokLambda p -> p
    TokFun p -> p
    TokArrow p -> p
    TokCase p -> p
    TokOf p -> p
    TokInteger p -> p
    TokChar p -> p
    TokUnit p -> p
    TokOpenParen p -> p
    TokCloseParen p -> p
    TokIntegerLiteral p _ -> p
    TokCharLiteral p _ -> p
    TokIdentifier p _ -> p
    TokOpenBlock p -> p
    TokCloseBlock p -> p
    TokSeparator p -> p
    TokColon p -> p
    TokDef p -> p
    TokEquals p -> p
    TokIn p -> p
    TokOpPlus p -> p
    TokOpMinus p -> p
    TokOpEquals p -> p
    TokOpLessEquals p -> p
    TokOpGreaterEquals p -> p
    TokFinal p -> p
    TokImmut p -> p
