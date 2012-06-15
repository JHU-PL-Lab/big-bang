{-# LANGUAGE DeriveDataTypeable #-}
module Language.TinyBang.Syntax.Lexer
( Token
, RawToken(..)
, getRawToken
, lexTinyBang
, matchesRawToken
, weaklyMatchesRawToken
, LexerResult
, getPos
, weakEq
, SourceLocation
, defaultSourceLocation
) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos (initialPos)
import Utils.Render.Display (Display, makeDoc, text)

import Data.Function (on)
import Data.Typeable (Typeable)
import Data.Data (Data, toConstr)

--FIXME: This should use a LexError type instead of a String.
type LexerResult = Either String [Token]

lexTinyBang :: String -> LexerResult
lexTinyBang s = case parse lexer "" s of
    Left x -> Left (show x)
    Right x -> Right $ map (\(start, t, end) -> Token (start, end) t) x


lexer :: Parser [(SourcePos, RawToken, SourcePos)]
lexer = do
    toks <- many (whitespaceP >> thing)
    whitespaceP >> eof
    return toks
    where
        thing = do
            start <- getPosition
            tok <- tryThemAll
            end <- getPosition
            return (start, tok, end)
        tryThemAll = choice $ map try $
                       concat
                       [ reservedWords
                       , longOperators
                       , hungry
                       , shortOperators
                       ]

whitespaceP :: Parser ()
whitespaceP = do
    -- Should this be skipMany1?
    skipMany space

validIdentP :: Parser ()
validIdentP = notFollowedBy (alphaNum <|> char '_')

hungry :: [Parser RawToken]
hungry = [identP, intLiteralP, charLiteralP]
    where
        identP = do
            first <- letter <|> char '_'
            rest <- many (alphaNum <|> char '_')
            validIdentP
            return $ TokIdentifier (first:rest)
        intLiteralP = do
            prefix <- option ' ' (char '-')
            digits <- many1 digit
            return $ TokIntegerLiteral (read (prefix:digits))
        charLiteralP = do
            let tick = char '\''
            l <- between tick tick anyChar
            return $ TokCharLiteral l

shortOperators :: [Parser RawToken]
shortOperators =
  map proc
    [ lblPrefixP
    , openParenP
    , closeParenP
    , openBlockP
    , closeBlockP
    , separatorP
    , colonP
    , opPlusP
    , opMinusP
    , equalsP
    , onionConsP
    ]
    where
      proc (c, t) = do
        _ <- char c
        return t
      lblPrefixP  = ('`', TokLabelPrefix)
      openParenP  = ('(', TokOpenParen)
      closeParenP = (')', TokCloseParen)
      openBlockP  = ('{', TokOpenBlock)
      closeBlockP = ('}', TokCloseBlock)
      separatorP  = (';', TokSeparator)
      colonP      = (':', TokColon)
      opPlusP     = ('+', TokOpPlus)
      opMinusP    = ('-', TokOpMinus)
      equalsP     = ('=', TokEquals)
      onionConsP  = ('&', TokOnionCons)

longOperators :: [Parser RawToken]
longOperators =
  map proc
    [ onionSubP
    , onionProjP
    , arrowP
    , opEqualsP
    , opLessEqualsP
    , opGreaterEqualsP
    ]
    where
      proc (s, t) = do
        _ <- string s
        return t
      onionSubP        = ("&-", TokOnionSub)
      onionProjP       = ("&.", TokOnionProj)
      arrowP           = ("->", TokArrow)
      opEqualsP        = ("==", TokOpEquals)
      opLessEqualsP    = ("<=", TokOpLessEquals)
      opGreaterEqualsP = (">=", TokOpGreaterEquals)

reservedWords :: [Parser RawToken]
reservedWords =
  map proc
    [ funP
    , intP
    , charP
    , unitP
    , defP
    , inP
    , finalP
    , immutP
    ]
    where
      proc (s, t) = do
        _ <- string s
        validIdentP
        return t
      funP   = ("fun"   , TokFun)
      intP   = ("int"   , TokInteger)
      charP  = ("char"  , TokChar)
      unitP  = ("unit"  , TokUnit)
      defP   = ("def"   , TokDef)
      inP    = ("in"    , TokIn)
      finalP = ("final" , TokFinal)
      immutP = ("immut" , TokImmut)

type SourceLocation = (SourcePos, SourcePos)
defaultSourceLocation :: SourceLocation
defaultSourceLocation = ((initialPos ""),(initialPos ""))

-- | This data type is used to specify the portion of the token that is
-- independent of the source location; it will be exported, but it cannot be
-- used to create a Token outside of this module because that constructor is not
-- exported.
data RawToken =
      TokLabelPrefix
    | TokOnionCons
    | TokOnionSub
    | TokOnionProj
    | TokFun
    | TokArrow
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
    deriving (Show, Typeable, Data)

-- | This data type is used to represent tokens and contains the source
-- location; it is exported, but its constructor is not, so `RawToken`s can't be
-- used to construct it.
data Token = Token SourceLocation RawToken
  deriving (Show)

instance Eq RawToken where
  t1 == t2 =
    case (t1,t2) of
      (TokIntegerLiteral i1,TokIntegerLiteral i2) -> i1 == i2
      (TokCharLiteral c1,TokCharLiteral c2) -> c1 == c2
      (TokIdentifier i1,TokIdentifier i2) -> i1 == i2
      -- In the case that none of the three above branches matched, the only
      -- information is contained in the constructor; `toConstr` is a method
      -- provided by the Data type class.
      _ -> toConstr t1 == toConstr t2

-- TODO: verify that this is the behavior we want.
instance Eq Token where
  -- Compares the two tokens by their raw token, ignoring the source location
  (==) = (==) `on` (\(Token _ rtok) -> rtok)

-- | Compares two `RawToken`s ignoring their payloads; only uses constructors.
rawWeakEq :: RawToken -> RawToken -> Bool
rawWeakEq = (==) `on` toConstr

-- | Compares two `Token`s ignoring their source locations and their raw tokens'
--   payloads (if they have any); only uses constructors.
weakEq :: Token -> Token -> Bool
weakEq (Token _ rtok1) (Token _ rtok2) = rawWeakEq rtok1 rtok2

-- | Determines if the contents of a `Token` match a `RawToken`. This is useful
--   for comparing a token from the stream against a "token" specified in
--   source.
matchesRawToken :: Token -> RawToken -> Bool
matchesRawToken (Token _ rtok) rtok' = rtok == rtok'

-- | Determines if the contents of a `Token` match a `RawToken`, ignoring
--   payload. This is useful for determining that a token in the stream has a
--   particular constructor.
weaklyMatchesRawToken :: Token -> RawToken -> Bool
weaklyMatchesRawToken (Token _ rtok) rtok' = rawWeakEq rtok rtok'

instance Display RawToken where
    makeDoc tok = text $ case tok of
        TokLabelPrefix -> "label prefix"
        TokOnionCons -> "onion constructor"
        TokOnionSub -> "onion subtractor"
        TokOnionProj -> "onion projector"
        TokFun -> "fun"
        TokArrow -> "arrow"
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

instance Display Token where
  -- Ignore source position in displaying
  makeDoc (Token _ tok) = makeDoc tok

getPos :: Token -> SourceLocation
getPos (Token loc _) = loc

getRawToken :: Token -> RawToken
getRawToken (Token _ rtok) = rtok